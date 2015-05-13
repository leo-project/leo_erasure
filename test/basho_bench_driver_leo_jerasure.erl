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
-module(basho_bench_driver_leo_jerasure).

-export([new/1,
         run/4]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
          coding :: atom(),
          coding_params :: {integer(), integer(), integer()},
          bin_size :: integer(),
          block_id_list :: [{binary(), integer()}],
          erasure :: integer()
         }).

new(_Id) ->
    Coding = basho_bench_config:get('coding', vandrs),
    CodingParams = basho_bench_config:get('coding_params', {4,2,8}),
    BinSize = basho_bench_config:get('bin_size', 10485760),
    Erasure = basho_bench_config:get('erasure', 0),

    BinSizeBits = BinSize * 8,
    Bin = << 0:BinSizeBits >>,
    {ok, BlockList} = leo_jerasure:encode(Bin, BinSize, Coding, CodingParams),
    N = length(BlockList),
    IdList = lists:seq(0, N - 1),
    BlockWithIdList = lists:zip(BlockList, IdList),

    ?debugFmt('Prepared Blocks for ~p ~p [~p Bytes]', [Coding, CodingParams, BinSize]),
    {ok, #state {coding = Coding,
                 coding_params = CodingParams,
                 bin_size = BinSize,
                 block_id_list = BlockWithIdList,
                 erasure = Erasure}}.

run(encode, _KeyGen, ValGen, #state{ coding = Coding, coding_params = CodingParams } = State) ->
    Val = ValGen(),
    case leo_jerasure:encode(Val, byte_size(Val), Coding, CodingParams) of
        {error, Cause} ->
            {error, Cause, State};
        _ ->
            {ok, State}
    end;

run(decode, _KeyGen, _ValGen, #state{coding = Coding, coding_params = CodingParams, 
                               bin_size = BinSize, block_id_list = BlockWithIdList,
                               erasure = Erasure } = State) ->
    {K, _, _} = CodingParams,
    Selected = lists:sublist(BlockWithIdList, Erasure + 1, Erasure + K),
    case leo_jerasure:decode(Selected, BinSize, Coding, CodingParams) of
        {error, Cause} ->
            {error, Cause, State};
        _ ->
            {ok, State}
    end.
