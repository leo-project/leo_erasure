%%======================================================================
%%
%% Leo Erasure Code
%%
%% Copyright (c) 2012-2015 Rakuten, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%%======================================================================
-module(leo_jerasure).

-export([encode_file/1,decode_file/2]).
-export([encode_file/3,decode_file/4]).
-export([write_blocks/3]).
-export([encode/4, decode/4, decode/5]).
-export([benchmark_encode/4]).

-on_load(init/0).

-define(BLOCKSTOR, "blocks/").

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.

-define(ECODE_CLASS, vandrs).
-define(ECODE_PARAMS, {10, 4, 8}).

%% @doc Initialize, Loading NIF Driver
%%
init() ->
    SoName = case code:priv_dir(?MODULE) of
                 {error, bad_name} ->
                     case code:which(?MODULE) of
                         Filename when is_list(Filename) ->
                             filename:join([filename:dirname(Filename),"../priv", "leo_jerasure"]);
                         _ ->
                             filename:join("../priv", "leo_jerasure")
                     end;
                 Dir ->
                     filename:join(Dir, "leo_jerasure")
             end,
    erlang:load_nif(SoName, 0),
    filelib:ensure_dir(?BLOCKSTOR).

%% @doc Write Blocks to Disk
%%
write_blocks(_, [], Cnt) ->
    Cnt;
write_blocks(FileName, [H | T], Cnt) ->
    filelib:ensure_dir(?BLOCKSTOR),
    BlockName = FileName ++ "." ++ integer_to_list(Cnt),
    BlockPath = filename:join(?BLOCKSTOR, BlockName),
    file:write_file(BlockPath, H),
    write_blocks(FileName, T, Cnt + 1).

%% @doc Encode a File to Blocks and Write to Disk
%%
encode_file(FileName) ->
    encode_file(FileName, ?ECODE_CLASS, ?ECODE_PARAMS).
encode_file(FileName, Coding, CodingParams) ->
    case file:read_file(FileName) of
        {ok, FileContent} ->
            io:format("File Content Length: ~p~n", [byte_size(FileContent)]),
            {Time, {ok, Blocks}} = timer:tc(?MODULE, encode, [FileContent, byte_size(FileContent),
                                                              Coding, CodingParams]),
            io:format("Duration ~p us~n", [Time]),
            io:format("Number of Blocks: ~p~n", [length(Blocks)]);
        {error, Reason} ->
            Blocks = [],
            erlang:error(Reason)
    end,
    filelib:ensure_dir(?BLOCKSTOR),
    write_blocks(FileName, Blocks, 0).

%% @doc Read Blocks from Disk, Decode the File and Write with .dec Extension
%%
decode_file(FileName, FileSize) ->
    decode_file(FileName, FileSize, ?ECODE_CLASS, ?ECODE_PARAMS).
decode_file(FileName, FileSize, Coding, CodingParams) ->
    AvailList = check_available_blocks(FileName, 14, []),
%    BlockList = read_blocks(FileName, AvailList),
    BlockWithIdList = read_blocks(FileName, AvailList),
%    {Time, {ok, FileContent}} = timer:tc(?MODULE, decode, [BlockList, AvailList, FileSize, Coding, CodingParams]),
    {Time, {ok, FileContent}} = timer:tc(?MODULE, decode, [BlockWithIdList, FileSize, Coding, CodingParams]),
    io:format("Duration ~p~n", [Time]),
    DecodeName = FileName ++ ".dec",
    io:format("Decoded file at ~p~n", [DecodeName]),
    file:write_file(DecodeName, FileContent).

%% @doc Benchmark Encoding Speed
%%
-spec(benchmark_encode(TotalSizeM, ChunkSizeM, Coding, Params) ->
            {ok, atom()} when TotalSizeM::integer(),
                              ChunkSizeM::integer(),
                              Coding::atom(),
                              Params::{integer(), integer(), integer()}).
benchmark_encode(TotalSizeM, ChunkSizeM, Coding, Params) ->
    TotalSize = TotalSizeM * 1024 * 1024,
    ChunkSize = ChunkSizeM * 1024 * 1024,
    ChunkSizeBits = ChunkSize * 8,
    Bin = << 0:ChunkSizeBits>>,
    Start = now(),
    repeat_encode(Bin, ChunkSize, Coding, Params, TotalSize div ChunkSize),
    End = now(),
    Time = timer:now_diff(End, Start),
    Rate = TotalSizeM / Time * 1000 * 1000,
    io:format("Encode Rate: ~p MB/s~n", [Rate]),
    {ok, Time}.

%% @doc Actual Encoding with Jerasure (NIF)
%%
-spec(encode(Bin, TotalSize, Coding, CodingParams) ->
            {ok, [binary()]} | {error, any()} when Bin::binary(),
                                                 TotalSize::integer(),
                                                 Coding::atom(),
                                                 CodingParams::{integer(), integer(), integer()}).
encode(_Bin,_TotalSize,_Coding,_CodingParams) ->
    exit(nif_library_not_loaded).

%% @doc Actual Decoding with Jerasure (NIF)
%%
-spec(decode(BlockList, IdList, FileSize, Coding, CodingParams) ->
            {ok, binary()} | {error, any()} when BlockList::[binary()],
                                                 IdList::[integer()],
                                                 FileSize::integer(),
                                                 Coding::atom(),
                                                 CodingParams::{integer(), integer(), integer()}).
decode(_BlockList,_IdList,_FileSize,_Coding,_CodingParams) ->
    exit(nif_library_not_loaded).

%% @doc Actual Decoding with Jerasure (NIF) [{ID, Bin}] Interface
%%
-spec(decode(BlockWithIdList, FileSize, Coding, CodingParams) ->
            {ok, binary()} | {error, any()} when BlockWithIdList::[{binary(), integer()}],
                                                 FileSize::integer(),
                                                 Coding::atom(),
                                                 CodingParams::{integer(), integer(), integer()}).
decode(BlockWithIdList, FileSize, Coding, CodingParams) ->
    SortFun = fun (A ,B) ->
                      {_BlockA, IdA} = A,
                      {_BlockB, IdB} = B,
                      IdA =< IdB
              end,
    SortedList = lists:sort(SortFun, BlockWithIdList),
    {BlockList, IdList} = lists:unzip(SortedList),
    decode(BlockList, IdList, FileSize, Coding, CodingParams).

%% @doc Repeat the Encoding Process
%% @private
repeat_encode(_, _, _, _, 0)->
    ok;
repeat_encode(Bin, BinSize, Coding, CodingParams, Cnt)->
    io:format("Encode Round Remained: ~p~n", [Cnt]),
    {ok, _} = encode(Bin, BinSize, Coding, CodingParams),
    repeat_encode(Bin, BinSize, Coding, CodingParams, Cnt - 1).

%% @doc Check which Blocks are Available on Disk
%% @private
check_available_blocks(_, -1, List) ->
    List;
check_available_blocks(FileName, Cnt, List) ->
    BlockName = FileName ++ "." ++ integer_to_list(Cnt),
    BlockPath = filename:join(?BLOCKSTOR, BlockName),
    case filelib:is_regular(BlockPath) of
        true ->
            check_available_blocks(FileName, Cnt - 1, [Cnt | List]);
        _ ->
            check_available_blocks(FileName, Cnt - 1, List)
    end.

%% @doc Read Blocks from disk
%% @private
read_blocks(FileName, AvailList) ->
%    read_blocks(FileName, lists:reverse(AvailList), []).
    read_blocks(FileName, AvailList, []).
read_blocks(_, [], BlockList) ->
    BlockList;
read_blocks(FileName, [Cnt | T], BlockList) ->
    BlockName = FileName ++ "." ++ integer_to_list(Cnt),
    BlockPath = filename:join(?BLOCKSTOR, BlockName),
    {ok, Block} = file:read_file(BlockPath),
    read_blocks(FileName, T, [{Block, Cnt} | BlockList]).

