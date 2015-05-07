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
-module(test_leo_jerasure).
-author("Wilson Li").

-include_lib("eunit/include/eunit.hrl").

-ifdef(EUNIT).
-define(TEST_SIZE, 10485760 + 1).

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

decode_test(Bin, BlockList, Coding, CodingParams, Failures) ->
    {K, M, _W} = CodingParams,
    FullList = lists:seq(0, K + M - 1),
    FailureCombs = comb(K + M - Failures, FullList),
    Func = fun(AvailList) ->
                   AvailBlocks = filter_block(BlockList, AvailList),
                   {ok, OutBin} = leo_jerasure:decode(AvailBlocks, AvailList, byte_size(Bin), Coding, CodingParams),
                   case OutBin of
                       Bin ->
                           ok;
                       _ ->
                           file:write_file("bin.ori", Bin),
                           file:write_file("bin.dec", OutBin),
                           leo_jerasure:write_blocks("bin", AvailBlocks, 0),
                           leo_jerasure:write_blocks("ori_bin", BlockList, 0),
                           erlang:error("Not Matched")
                   end
           end,
    lists:foreach(Func, FailureCombs),
    ok.

correctness_test(Bin, Coding, CodingParams, Failures) ->
    ?debugFmt(" =====   ~p ~p with ~p failures (all cases)", [Coding, CodingParams, Failures]),
    {ok, BlockList} = leo_jerasure:encode(Bin, byte_size(Bin), Coding, CodingParams),
    ok = decode_test(Bin, BlockList, Coding, CodingParams, Failures).

bench_encode_test() ->
    ?debugMsg(" ===== Encoding Benchmark Test ====="),
    bench_encode(vandrs,{10,4,8}),
    bench_encode(cauchyrs,{10,4,10}),
    bench_encode(liberation,{10,2,11}).

parameters_test() ->
    ?debugMsg(" ===== Testing Parameters ====="),
    Bin = crypto:rand_bytes(1024),
    ?debugMsg(" =====   Invalid: vandrs {4,2,7}"),
    {error, _} = leo_jerasure:encode(Bin, byte_size(Bin), vandrs, {4,2,7}),

    ?debugMsg(" =====   Invalid: vandrs {4,2,8} with 3 failures"),
    {ok, BlockList} = leo_jerasure:encode(Bin, byte_size(Bin), vandrs, {4,2,8}),
    {error, _} = leo_jerasure:decode(BlockList,[0,1,2], byte_size(Bin), vandrs, {4,2,8}),

    ?debugMsg(" =====   Invalid: cauchyrs {10,4,3}"),
    {error, _} = leo_jerasure:encode(Bin, byte_size(Bin), cauchyrs, {10,4,3}),

    ?debugMsg(" =====   Invalid: cauchyrs {4,2,3} with 3 failures"),
    {ok, BlockList2} = leo_jerasure:encode(Bin, byte_size(Bin), cauchyrs, {4,2,3}),
    {error, _} = leo_jerasure:decode(BlockList2,[0,1,2], byte_size(Bin), cauchyrs, {4,2,3}),

    ?debugMsg(" =====   Invalid: liberation {4,2,6}"),
    {error, _} = leo_jerasure:encode(Bin, byte_size(Bin), liberation, {4,2,6}),

    ?debugMsg("=====   Invalid: liberation {4,2,3}"),
    {error, _} = leo_jerasure:encode(Bin, byte_size(Bin), liberation, {4,2,3}),

    ?debugMsg("=====   Invalid: liberation {4,2,5} with 3 failures"),
    {ok, BlockList3} = leo_jerasure:encode(Bin, byte_size(Bin), liberation, {4,2,5}),
    {error, _} = leo_jerasure:decode(BlockList3,[0,1,2], byte_size(Bin), liberation, {4,2,5}),

    ?debugMsg("=====   Invalid: Unknown {4,2,3}"),
    {error, _} = leo_jerasure:encode(Bin, byte_size(Bin), unkown, {4,2,3}),

    ?debugMsg("=====   Invalid: liberation {troll}"),
    {error, _} = leo_jerasure:encode(Bin, byte_size(Bin), liberation, {troll}),

    ?debugMsg("=====   Invalid: {4,2,5}, liberation"),
    {error, _} = leo_jerasure:encode(Bin, byte_size(Bin), {4,2,5}, liberation).

suite_test_() ->
    {timeout, 180, fun long_process/0}.

file_test() ->
    ?debugMsg("===== Testing encode_file + decode_file ====="),
    Bin = crypto:rand_bytes(?TEST_SIZE),
    ?debugFmt("=====   vandrs {10,4,8} ~p bytes", [?TEST_SIZE]), 
    file:write_file("testbin", Bin),
    leo_jerasure:encode_file("testbin", vandrs, {10,4,8}),
    ?debugMsg("=====   Erasure Block 0,2,4,6"),
    file:delete("blocks/testbin.0"),
    file:delete("blocks/testbin.2"),
    file:delete("blocks/testbin.4"),
    file:delete("blocks/testbin.6"),
    leo_jerasure:decode_file("testbin", ?TEST_SIZE, vandrs, {10,4,8}),
    {ok, DecBin} = file:read_file("testbin.dec"),
    ?assertEqual(Bin, DecBin),
    ?debugMsg("=====   Correct, Cleanup"),
    BlockPathList = filelib:wildcard("blocks/testbin.*"),
    lists:foreach(fun file:delete/1, BlockPathList),
    file:delete("testbin"),
    file:delete("testbin.dec").

long_process() ->
    ?debugMsg("===== Testing Encode + Decode ====="),
    Bin = crypto:rand_bytes(?TEST_SIZE),
    correctness_test(Bin, vandrs, {4,2,8}, 1),
    correctness_test(Bin, vandrs, {4,2,8}, 2),
    correctness_test(Bin, vandrs, {10,4,8}, 4),
    correctness_test(Bin, cauchyrs, {4,2,3}, 1),
    correctness_test(Bin, cauchyrs, {4,2,3}, 2),
    correctness_test(Bin, liberation, {4,2,7}, 1),
    correctness_test(Bin, liberation, {4,2,7}, 2).

bench_encode(Coding, CodingParams) ->
    {ok, Time} = leo_jerasure:benchmark_encode(100, 100, Coding, CodingParams),
    Rate = 100 / Time * 1000 * 1000,
    ?debugFmt("=====   ~p ~p Encoding Rate: ~p MB/s", [Coding, CodingParams, Rate]).

-endif.
