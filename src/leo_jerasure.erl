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

-export([encode_file/1, encode_file/3,
         decode_file/2, decode_file/4, write_blocks/3]).
-export([encode/3, encode/4,
         decode/4, decode/5,
         repair/4, repair/5]).
-export([benchmark_encode/4]).

-on_load(init/0).

-define(BLOCKSTOR, "blocks/").

-include("leo_jerasure.hrl").
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Initialize, Loading NIF Driver
-spec(init() ->
             ok | {error, Reason} when Reason::file:posix()).
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
-spec(write_blocks(FileName, DestFileL, Cnt) ->
             Cnt when FileName::file:filename(),
                      DestFileL::[file:filename()],
                      Cnt::pos_integer()).
write_blocks(_, [], Cnt) ->
    Cnt;
write_blocks(FileName, [H|T], Cnt) ->
    filelib:ensure_dir(?BLOCKSTOR),
    BlockName = FileName ++ "." ++ integer_to_list(Cnt),
    BlockPath = filename:join(?BLOCKSTOR, BlockName),
    file:write_file(BlockPath, H),
    write_blocks(FileName, T, Cnt + 1).


%% @doc Encode a File to Blocks and Write to Disk
-spec(encode_file(FileName) ->
             Cnt | {error, Reason} when FileName::file:filename(),
                                        Cnt::pos_integer(),
                                        Reason::file:posix()).
encode_file(FileName) ->
    encode_file(FileName, ?DEF_CODING_CLASS, ?DEF_CODING_PARAMS).

-spec(encode_file(FileName, Coding, CodingParams) ->
             Cnt | {error, Reason} when FileName::file:filename(),
                                        Coding::coding_class(),
                                        CodingParams::coding_params(),
                                        Cnt::pos_integer(),
                                        Reason::any()).
encode_file(FileName, Coding, CodingParams) ->
    case file:read_file(FileName) of
        {ok, FileContent} ->
            case encode(Coding, CodingParams, FileContent) of
            %% case encode(Coding, CodingParams, FileContent, byte_size(FileContent)) of
                {ok, Blocks} ->
                    case filelib:ensure_dir(?BLOCKSTOR) of
                        ok ->
                            write_blocks(FileName, Blocks, 0);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Why} ->
                    {error, Why}
            end;
        {error, Cause} ->
            {error, Cause}
    end.


%% @doc Read Blocks from Disk, Decode the File and Write with .dec Extension
-spec(decode_file(FileName, FileSize) ->
             ok | {error, Reason} when FileName::file:filename(),
                                       FileSize::non_neg_integer(),
                                       Reason::any()).
decode_file(FileName, FileSize) ->
    decode_file(FileName, FileSize, ?DEF_CODING_CLASS, ?DEF_CODING_PARAMS).

-spec(decode_file(FileName, FileSize, Coding, CodingParams) ->
             ok | {error, Reason} when FileName::file:filename(),
                                       FileSize::non_neg_integer(),
                                       Coding::coding_class(),
                                       CodingParams::coding_params(),
                                       Reason::any()).
decode_file(FileName, FileSize, Coding, CodingParams) ->
    AvailList = check_available_blocks(FileName, 14, []),
    BlockWithIdList = read_blocks(FileName, AvailList),

    case decode(BlockWithIdList, FileSize, Coding, CodingParams) of
        {ok, Bin} ->
            file:write_file(FileName ++ ".dec", Bin);
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc Benchmark Encoding Speed
-spec(benchmark_encode(TotalSizeM, ChunkSizeM, Coding, Params) ->
        {ok, atom()} when TotalSizeM::integer(),
                          ChunkSizeM::integer(),
                          Coding::atom(),
                          Params::{integer(), integer(), integer()}).
benchmark_encode(TotalSizeM, ChunkSizeM, Coding, Params) ->
    KBytes = 1024 * 1024,
    TotalSize = TotalSizeM * KBytes,
    ChunkSize = ChunkSizeM * KBytes,
    ChunkSizeBits = ChunkSize * 8,
    Bin = << 0:ChunkSizeBits>>,
    Start = os:timestamp(),
    repeat_encode(Bin, ChunkSize, Coding, Params, (TotalSize div ChunkSize)),
    End = os:timestamp(),
    Time = timer:now_diff(End, Start),
    {ok, Time}.


%% @doc Actual Encoding with Jerasure (NIF)
-spec(encode(CodingClass, CodingParams, Bin) ->
        {ok, Blocks} | {error, any()} when CodingClass::coding_class(),
                                           CodingParams::coding_params(),
                                           Bin::binary(),
                                           Blocks::[binary()]).
encode(CodingClass, {Coding_K, Coding_M, Coding_W}, Bin) when Coding_W < 1 ->
    encode(CodingClass, {Coding_K, Coding_M, ?coding_params_w(CodingClass)}, Bin);
encode(CodingClass, CodingParams, Bin) ->
    encode(CodingClass, CodingParams, Bin, byte_size(Bin)).

-spec(encode(CodingClass, CodingParams, Bin, TotalSize) ->
        {ok, Blocks} | {error, any()} when CodingClass::coding_class(),
                                           CodingParams::coding_params(),
                                           Bin::binary(),
                                           TotalSize::integer(),
                                           Blocks::[binary()]).
encode(_CodingClass,_CodingParams,_Bin,_TotalSize) ->
    exit(nif_library_not_loaded).


%% @doc Actual Decoding with Jerasure (NIF)
-spec(decode(BlockL, IdList, FileSize, CodingClass, CodingParams) ->
             {ok, Bin} | {error, any()} when BlockL::[binary()],
                                             IdList::[integer()],
                                             FileSize::integer(),
                                             CodingClass::coding_class(),
                                             CodingParams::coding_params(),
                                             Bin::binary()).
decode(_BlockL,_IdList,_FileSize,_CodingClass,_CodingParams) ->
    exit(nif_library_not_loaded).

-spec(decode(BlockWithIdList, FileSize, CodingClass, CodingParams) ->
        {ok, Bin} | {error, any()} when BlockWithIdList::[{binary(), integer()}],
                                        FileSize::integer(),
                                        CodingClass::coding_class(),
                                        CodingParams::coding_params(),
                                        Bin::binary()).
decode(BlockWithIdList, FileSize, CodingClass, CodingParams) ->
    %% Actual Decoding with Jerasure (NIF) [{ID, Bin}] Interface
    {BlockL, IdList} = lists:unzip(BlockWithIdList),
    decode(BlockL, IdList, FileSize, CodingClass, CodingParams).


%% @doc Repair Multiple Blocks with Jerasure (NIF)
%%
-spec(repair(BlockL, IdList, RepairIdList, CodingClass, CodingParams) ->
             {ok, BlockL} | {error, any()} when BlockL::[binary()],
                                                IdList::[integer()],
                                                RepairIdList ::[integer()],
                                                CodingClass::coding_class(),
                                                CodingParams::coding_params()).
repair(_BlockL,_IdList,_RepairIdList,_CodingClass,_CodingParams) ->
    exit(nif_library_not_loaded).

-spec(repair(BlockWithIdList, RepairIdList, CodingClass, CodingParams) ->
             {ok, BlockL} | {error, any()} when BlockWithIdList::[{binary(), integer()}],
                                                RepairIdList ::[integer()],
                                                CodingClass::coding_class(),
                                                CodingParams::coding_params(),
                                                BlockL::[binary()]).
repair(BlockWithIdList, RepairIdList, CodingClass, CodingParams) ->
    %% Repair Multiple Blocks with Jerasure (NIF) [{Bin, Id}] Interface
    {BlockL, IdList} = lists:unzip(BlockWithIdList),
    repair(BlockL, IdList, RepairIdList, CodingClass, CodingParams).


%%--------------------------------------------------------------------
%% INNTERNAL FUNCTIONS
%%--------------------------------------------------------------------
%% @doc Repeat the Encoding Process
%% @private
repeat_encode(_, _, _, _, 0)->
    ok;
repeat_encode(Bin, BinSize, CodingClass, CodingParams, Cnt)->
    io:format("Encode Round Remained: ~p~n", [Cnt]),
    {ok, _} = encode(CodingClass, CodingParams, Bin),
    %% {ok, _} = encode(CodingClass, CodingParams, Bin, BinSize),
    repeat_encode(Bin, BinSize, CodingClass, CodingParams, Cnt - 1).

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
    read_blocks(FileName, AvailList, []).
read_blocks(_, [], BlockL) ->
    BlockL;
read_blocks(FileName, [Cnt | T], BlockL) ->
    BlockName = FileName ++ "." ++ integer_to_list(Cnt),
    BlockPath = filename:join(?BLOCKSTOR, BlockName),
    {ok, Block} = file:read_file(BlockPath),
    read_blocks(FileName, T, [{Block, Cnt} | BlockL]).
