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
-module(leo_erasure).

-export([encode_file/1, encode_file/3,
         decode_file/2, decode_file/4, write_blocks/3]).
-export([encode/2, encode/3, encode/4,
         decode/3, decode/4, decode/5,
         repair/2, repair/3, repair/5]).

-on_load(init/0).

-define(BLOCKSTOR, "blocks/").

-include("leo_erasure.hrl").
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
                             filename:join([filename:dirname(Filename),"../priv", "leo_erasure"]);
                         _ ->
                             filename:join("../priv", "leo_erasure")
                     end;
                 Dir ->
                     filename:join(Dir, "leo_erasure")
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
    encode_file(?DEF_CODING_CLASS, ?DEF_CODING_PARAMS, FileName).

-spec(encode_file(Coding, CodingParams, FileName) ->
             Cnt | {error, Reason} when Coding::coding_class(),
                                        CodingParams::coding_params(),
                                        FileName::file:filename(),
                                        Cnt::pos_integer(),
                                        Reason::any()).
encode_file(Coding, CodingParams, FileName) ->
    case file:read_file(FileName) of
        {ok, Bin} ->
            case encode(Coding, CodingParams, Bin, byte_size(Bin)) of
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
-spec(decode_file(FileName, ObjSize) ->
             ok | {error, Reason} when FileName::file:filename(),
                                       ObjSize::non_neg_integer(),
                                       Reason::any()).
decode_file(FileName, ObjSize) ->
    decode_file(?DEF_CODING_CLASS, ?DEF_CODING_PARAMS, FileName, ObjSize).

-spec(decode_file(CodingClass, CodingParams, FileName, ObjSize) ->
             ok | {error, Reason} when CodingClass::coding_class(),
                                       CodingParams::coding_params(),
                                       FileName::file:filename(),
                                       ObjSize::non_neg_integer(),
                                       Reason::any()).
decode_file(CodingClass, CodingParams, FileName, ObjSize) ->
    AvailList = check_available_blocks(FileName, 14, []),
    IdWithBlockL = read_blocks(FileName, AvailList),

    case decode(CodingClass, CodingParams, IdWithBlockL, ObjSize) of
        {ok, Bin} ->
            file:write_file(FileName ++ ".dec", Bin);
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc Actual Encoding with Jerasure (NIF)
-spec(encode({CodingParam_K, CodingParam_M}, Bin) ->
             {ok, IdWithBlockL} | {error, any()} when CodingParam_K::pos_integer(),
                                                      CodingParam_M::pos_integer(),
                                                      Bin::binary(),
                                                      IdWithBlockL::[id_with_block()]).
encode({CodingParam_K, CodingParam_M}, Bin) ->
    CodingClass = ?DEF_CODING_CLASS,
    CodingParams = {CodingParam_K, CodingParam_M, ?coding_params_w(CodingClass)},
    encode(CodingClass, CodingParams, Bin).

-spec(encode(CodingClass, CodingParams, Bin) ->
             {ok, IdWithBlockL} | {error, any()} when CodingClass::coding_class(),
                                                      CodingParams::coding_params(),
                                                      Bin::binary(),
                                                      IdWithBlockL::[id_with_block()]).
encode(CodingClass, {CodingParam_K, CodingParam_M, Coding_W}, Bin) when Coding_W < 1 ->
    encode(CodingClass, {CodingParam_K, CodingParam_M, ?coding_params_w(CodingClass)}, Bin);
encode(CodingClass, CodingParams, Bin) ->
    case encode(CodingClass, CodingParams, Bin, byte_size(Bin)) of
        {ok, BlockL} ->
            {ok, lists:zip(lists:seq(0, length(BlockL)-1), BlockL)};
        Error ->
            Error
    end.

-spec(encode(CodingClass, CodingParams, Bin, TotalSize) ->
             {ok, Blocks} | {error, any()} when CodingClass::coding_class(),
                                                CodingParams::coding_params(),
                                                Bin::binary(),
                                                TotalSize::integer(),
                                                Blocks::[binary()]).
encode(_CodingClass,_CodingParams,_Bin,_TotalSize) ->
    exit(nif_library_not_loaded).


%% @doc Actual Decoding with Jerasure (NIF)
-spec(decode({CodingParam_K, CodingParam_M}, IdWithBlockL, ObjSize) ->
             {ok, Bin} | {error, any()} when CodingParam_K::pos_integer(),
                                             CodingParam_M::pos_integer(),
                                             IdWithBlockL::[id_with_block()],
                                             ObjSize::integer(),
                                             Bin::binary()).
decode({CodingParam_K, CodingParam_M}, IdWithBlockL, ObjSize) ->
    CodingClass = ?DEF_CODING_CLASS,
    CodingParams = {CodingParam_K, CodingParam_M, ?coding_params_w(CodingClass)},
    decode(CodingClass, CodingParams, IdWithBlockL, ObjSize).

-spec(decode(CodingClass, CodingParams, BlockL, IdList, ObjSize) ->
             {ok, Bin} | {error, any()} when CodingClass::coding_class(),
                                             CodingParams::coding_params(),
                                             BlockL::[binary()],
                                             IdList::[integer()],
                                             ObjSize::integer(),
                                             Bin::binary()).
decode(_CodingClass,_CodingParams,_BlockL,_IdList,_ObjSize) ->
    exit(nif_library_not_loaded).

-spec(decode(CodingClass, CodingParams, IdWithBlockL, ObjSize) ->
             {ok, Bin} | {error, any()} when CodingClass::coding_class(),
                                             CodingParams::coding_params(),
                                             IdWithBlockL::[id_with_block()],
                                             ObjSize::integer(),
                                             Bin::binary()).
decode(CodingClass, {CodingParam_K, CodingParam_M, Coding_W}, IdWithBlockL, ObjSize) when Coding_W < 1 ->
    decode(CodingClass,{CodingParam_K, CodingParam_M, ?coding_params_w(CodingClass)},
           IdWithBlockL, ObjSize);
decode(CodingClass, CodingParams, IdWithBlockL, ObjSize) ->
    {IdList, BlockL} = lists:unzip(IdWithBlockL),
    decode(CodingClass, CodingParams, BlockL, IdList, ObjSize).


%% @doc Repair Multiple Blocks with Jerasure (NIF)
%%
-spec(repair({CodingParam_K, CodingParam_M}, IdWithBlockL) ->
             {ok, IdWithBlockL} | {error, any()} when CodingParam_K::pos_integer(),
                                                      CodingParam_M::pos_integer(),
                                                      IdWithBlockL::[id_with_block()]).
repair({CodingParam_K, CodingParam_M}, IdWithBlockL) ->
    CodingClass = ?DEF_CODING_CLASS,
    CodingParams = {CodingParam_K, CodingParam_M, ?coding_params_w(CodingClass)},
    repair(CodingClass, CodingParams, IdWithBlockL).

-spec(repair(CodingClass, CodingParams, IdWithBlockL) ->
             {ok, IdWithBlockL} | {error, any()} when CodingClass::coding_class(),
                                                      CodingParams::coding_params(),
                                                      IdWithBlockL::[id_with_block()]).
repair(CodingClass, CodingParams, IdWithBlockL) ->
    {CodingParam_K, CodingParam_M,_CodingParam_W} = CodingParams,
    {IdList, BlockL} = lists:unzip(IdWithBlockL),
    CompleteList = lists:seq(0, CodingParam_K + CodingParam_M - 1),
    RepairIdList = lists:subtract(CompleteList, IdList),

    case repair(CodingClass, CodingParams, BlockL, IdList, RepairIdList) of
        {ok, RepairedBlockL} ->
            {ok, lists:zip(RepairIdList, RepairedBlockL)};
        Error ->
            Error
    end.

-spec(repair(CodingClass, CodingParams, BlockL, IdList, RepairIdList) ->
             {ok, BlockL} | {error, any()} when CodingClass::coding_class(),
                                                CodingParams::coding_params(),
                                                BlockL::[binary()],
                                                IdList::[non_neg_integer()],
                                                RepairIdList ::[non_neg_integer()]).
repair(_CodingClass,_CodingParams,_BlockL,_IdList,_RepairIdList) ->
    exit(nif_library_not_loaded).


%%--------------------------------------------------------------------
%% INNTERNAL FUNCTIONS
%%--------------------------------------------------------------------
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
    read_blocks(FileName, T, [{Cnt, Block} | BlockL]).
