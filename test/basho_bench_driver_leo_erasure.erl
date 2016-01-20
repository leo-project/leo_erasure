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
-module(basho_bench_driver_leo_erasure).

-export([new/1,
         run/4]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
          coding :: atom(),
          coding_params :: {integer(), integer(), integer()},
          bin :: binary(),
          bin_size :: integer(),
          block_id_list :: [{binary(), integer()}],
          erasure :: integer(),
          decode_comb :: [[integer()]],
          decode_comb_size :: integer()
         }).

filter_block(_BlockList, _Cnt, [], Acc) ->
    Acc;
filter_block([HB | TB], Cnt, [HF | TF] = FilterList, Acc) ->
    case Cnt of
        HF ->
            filter_block(TB, Cnt + 1, TF, Acc ++ [HB]);
        _ ->
            filter_block(TB, Cnt + 1, FilterList, Acc)
    end.
filter_block(BlockList, FilterList) ->
    filter_block(BlockList, 0, FilterList, []).

comb(0,_) ->
    [[]];
comb(_,[]) ->
    [];
comb(N,[H|T]) ->
    [[H|L] || L <- comb(N-1,T)]++comb(N,T).

new(_Id) ->
    Coding = basho_bench_config:get('coding', vandrs),
    CodingParams = basho_bench_config:get('coding_params', {4,2,8}),
    BinSize = basho_bench_config:get('bin_size', 10485760),
    Erasure = basho_bench_config:get('erasure', 0),

    BinSizeBits = BinSize * 8,
    Bin = << 0:BinSizeBits >>,
    {ok, BlockList} = leo_erasure:encode(Coding, CodingParams, Bin, BinSize),
    N = length(BlockList),
    IdList = lists:seq(0, N - 1),
    IdWithBlockList = lists:zip(IdList, BlockList),

    {K, M, _} = CodingParams,
    FullList = lists:seq(0, K + M - 1),
    DecodeCombs = comb(K, FullList),
    DecodeCombSize = length(DecodeCombs),

    ?debugFmt('Prepared Blocks for ~p ~p [~p Bytes]', [Coding, CodingParams, BinSize]),
    {ok, #state {coding = Coding,
                 coding_params = CodingParams,
                 bin = Bin,
                 bin_size = BinSize,
                 block_id_list = IdWithBlockList,
                 erasure = Erasure,
                 decode_comb = DecodeCombs,
                 decode_comb_size = DecodeCombSize}}.

run(encode, KeyGen, ValGen, #state{ coding = Coding, coding_params = CodingParams } = State) ->
    _Key = KeyGen(),
    Val = ValGen(),
    case leo_erasure:encode(Coding, CodingParams, Val, byte_size(Val)) of
        {error, Cause} ->
            {error, Cause, State};
        _ ->
            {ok, State}
    end;

run(decode, KeyGen, _ValGen, #state{coding = Coding, coding_params = CodingParams,
                                    bin_size = BinSize, block_id_list = IdWithBlockList,
                                    decode_comb = DecodeCombs, decode_comb_size = DecodeCombSize } = State) ->
    Key = KeyGen(),
    AvailList = lists:nth(Key rem DecodeCombSize + 1, DecodeCombs),
    Selected = filter_block(IdWithBlockList, AvailList),
    case leo_erasure:decode(Coding, CodingParams, Selected, BinSize) of
        {error, Cause} ->
            {error, Cause, State};
        _ ->
            {ok, State}
    end;

run(repair, KeyGen, _ValGen, #state{coding = Coding, coding_params = CodingParams,
                                    block_id_list = IdWithBlockList, erasure = Erasure } = State) ->
    Key = KeyGen(),
    {K, M, _} = CodingParams,
    FullList = lists:seq(0, K + M - 1),
    RepairList = [N rem (K + M) || N <- lists:seq(Key, Key + Erasure - 1)],
    RemainList = lists:subtract(FullList, RepairList),
    AvailList = lists:sublist(RemainList, 1, K),
    Selected = filter_block(IdWithBlockList, AvailList),
    case leo_erasure:repair(Coding, CodingParams, Selected, RepairList) of
        {ok, _Blocks} ->
            {ok, State};
        {error, Cause} ->
            {error, Cause, State}
    end.
