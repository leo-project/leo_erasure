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

repair_test(Bin, CodingClass, CodingParams, Erasure) ->
    ?debugFmt(" * ~p ~p with ~p failures(all cases)", [CodingClass, CodingParams, Erasure]),
    {ok, BlockList} = leo_jerasure:encode(CodingClass, CodingParams, Bin, byte_size(Bin)),
    {K, M, _W} = CodingParams,
    FullList = lists:seq(0, K + M - 1),
    ErasureCombs = comb(Erasure, FullList),
    Func = fun(RepairList) ->
                   AvailList = lists:subtract(FullList, RepairList),
                   AvailBlocks = filter_block(BlockList, AvailList),
                   LostBlocks = filter_block(BlockList, RepairList),
                   {ok, LostBlocks} = leo_jerasure:repair(AvailBlocks, AvailList, RepairList, CodingClass, CodingParams)
           end,
    _ = erlang:statistics(wall_clock),
    lists:foreach(Func, ErasureCombs),
    {_,Time} = erlang:statistics(wall_clock),
    ?debugFmt("   >> time: ~wms", [Time]),
    ok.

decode_test(Bin, BlockList, CodingClass, CodingParams, Failures) ->
    {K, M, _W} = CodingParams,
    FullList = lists:seq(0, K + M - 1),
    FailureCombs = comb(K + M - Failures, FullList),
    Func = fun(AvailList) ->
                   AvailBlocks = filter_block(BlockList, AvailList),
                   BlockIdList = AvailBlocks,
                   ShuffleList = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- BlockIdList])],
                   {ok, OutBin} = leo_jerasure:decode(CodingClass, CodingParams, ShuffleList, byte_size(Bin)),
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
    _ = erlang:statistics(wall_clock),
    lists:foreach(Func, FailureCombs),
    {_,Time} = erlang:statistics(wall_clock),
    ?debugFmt("   >> time: ~wms", [Time]),
    ok.

correctness_test(Bin, CodingClass, CodingParams, Failures) ->
    {K, M,_W} = CodingParams,
    ?debugFmt(" * ~p, {k:~w, m:~w} with ~p failures (all cases)", [CodingClass, K, M, Failures]),
    {ok, IdWithBlockL} = leo_jerasure:encode(CodingClass, CodingParams, Bin),
    ?assertEqual(K + M, erlang:length(IdWithBlockL)),
    ok = decode_test(Bin, IdWithBlockL, CodingClass, CodingParams, Failures).

bench_encode_test() ->
    ?debugMsg(" ===== Encoding Benchmark Test ====="),
    bench_encode(vandrs,{10,4,8}),
    bench_encode(cauchyrs,{10,4,10}),
    bench_encode(liberation,{10,2,11}).

parameters_test() ->
    ?debugMsg(" ===== Testing Parameters ====="),
    Bin = crypto:rand_bytes(1024),
    ?debugMsg(" * Invalid: vandrs {4,2,7}"),
    {error, _} = leo_jerasure:encode(vandrs, {4,2,7}, Bin),

    ?debugMsg(" * Invalid: vandrs {4,2,8} with 3 failures"),
    {ok, IdWithBlockL_1} = leo_jerasure:encode(vandrs, {4,2,8}, Bin),
    BlockList_1 = [B1 || {_,B1} <- IdWithBlockL_1],
    {error, _} = leo_jerasure:decode(vandrs, {4,2,8}, BlockList_1, [0,1,2], byte_size(Bin)),

    ?debugMsg(" * Invalid: cauchyrs {10,4,3}"),
    {error, _} = leo_jerasure:encode(cauchyrs, {10,4,3}, Bin),

    ?debugMsg(" * Invalid: cauchyrs {4,2,3} with 3 failures"),
    {ok, IdWithBlockL_2} = leo_jerasure:encode(cauchyrs, {4,2,3}, Bin),
    BlockList_2 = [B2 || {_,B2} <- IdWithBlockL_2],
    {error, _} = leo_jerasure:decode(cauchyrs, {4,2,3}, BlockList_2, [0,1,2], byte_size(Bin)),

    ?debugMsg(" * Invalid: liberation {4,2,6}"),
    {error, _} = leo_jerasure:encode(liberation, {4,2,6}, Bin),

    ?debugMsg(" * Invalid: liberation {4,2,3}"),
    {error, _} = leo_jerasure:encode(liberation, {4,2,3}, Bin),

    ?debugMsg(" * Invalid: liberation {4,2,5} with 3 failures"),
    {ok, IdWithBlockL_3} = leo_jerasure:encode(liberation, {4,2,5}, Bin),
    BlockList_3 = [B3 || {_,B3} <- IdWithBlockL_3],
    {error, _} = leo_jerasure:decode(liberation, {4,2,5}, BlockList_3, [0,1,2], byte_size(Bin)),

    ?debugMsg(" * Invalid: Unknown {4,2,3}"),
    {error, _} = leo_jerasure:encode(unkown, {4,2,3}, Bin),

    ?debugMsg(" * Invalid: liberation {troll}"),
    {error, _} = leo_jerasure:encode(liberation, {troll}, Bin),

    ?debugMsg(" * Invalid: {4,2,5}, liberation"),
    {error, _} = leo_jerasure:encode({4,2,5}, liberation, Bin),

    ?debugMsg(" * Simple encoder"),
    {ok, IdWithBlockL_4} = leo_jerasure:encode({10,4}, Bin),
    {ok, IdWithBlockL_5} = leo_jerasure:encode({8, 3}, Bin),
    {ok, IdWithBlockL_6} = leo_jerasure:encode({6, 2}, Bin),
    ?assertEqual(14, length(IdWithBlockL_4)),
    ?assertEqual(11, length(IdWithBlockL_5)),
    ?assertEqual(8,  length(IdWithBlockL_6)),

    {ok, Obj_4} = leo_jerasure:decode({10,4}, IdWithBlockL_4, byte_size(Bin)),
    {ok, Obj_5} = leo_jerasure:decode({8,3},  IdWithBlockL_5, byte_size(Bin)),
    {ok, Obj_6} = leo_jerasure:decode({6, 2}, IdWithBlockL_6, byte_size(Bin)),
    ?assertEqual(Bin, Obj_4),
    ?assertEqual(Bin, Obj_5),
    ?assertEqual(Bin, Obj_6),
    ok.

suite_test_() ->
    {timeout, 180, fun long_process/0}.

file_test() ->
    ?debugMsg("===== Testing encode_file + decode_file ====="),
    Bin = crypto:rand_bytes(?TEST_SIZE),
    ?debugFmt(" * vandrs {10,4,8} ~p bytes", [?TEST_SIZE]),
    file:write_file("testbin", Bin),
    leo_jerasure:encode_file(vandrs, {10,4,8}, "testbin"),
    ?debugMsg(" * Erasure Block 0,2,4,6"),
    file:delete("blocks/testbin.0"),
    file:delete("blocks/testbin.2"),
    file:delete("blocks/testbin.4"),
    file:delete("blocks/testbin.6"),
    leo_jerasure:decode_file(vandrs, {10,4,8}, "testbin", ?TEST_SIZE),
    {ok, DecBin} = file:read_file("testbin.dec"),
    ?assertEqual(Bin, DecBin),
    ?debugMsg(" * Correct, Cleanup"),
    BlockPathList = filelib:wildcard("blocks/testbin.*"),
    lists:foreach(fun file:delete/1, BlockPathList),
    file:delete("testbin"),
    file:delete("testbin.dec").

repair_test() ->
    ?debugMsg("===== Block Repair ====="),
    Bin = crypto:rand_bytes(?TEST_SIZE),
    repair_test(Bin, vandrs, {10,4,8}, 2),
    repair_test(Bin, cauchyrs, {4,2,3}, 2),
    repair_test(Bin, liberation, {4,2,7}, 2).

long_process() ->
    ?debugMsg("===== Testing Encode + Decode ====="),
    Bin = crypto:rand_bytes(?TEST_SIZE),
    correctness_test(Bin, vandrs, {10, 4,-1}, 1),
    correctness_test(Bin, cauchyrs, {4, 2,-1}, 1),
    correctness_test(Bin, liberation, {4, 2,-1}, 1),

    correctness_test(Bin, vandrs, {10, 4, 0}, 1),
    correctness_test(Bin, cauchyrs, {4, 2, 0}, 1),
    correctness_test(Bin, liberation, {4, 2, 0}, 1),

    correctness_test(Bin, vandrs, {4,2,8}, 0),
    correctness_test(Bin, vandrs, {4,2,8}, 1),
    correctness_test(Bin, vandrs, {4,2,8}, 2),
    correctness_test(Bin, vandrs, {8,3,8}, 0),
    correctness_test(Bin, vandrs, {8,3,8}, 1),
    correctness_test(Bin, vandrs, {8,3,8}, 2),
    correctness_test(Bin, vandrs, {8,3,8}, 3),
    correctness_test(Bin, vandrs, {10,4,8}, 0),
    correctness_test(Bin, vandrs, {10,4,8}, 1),
    correctness_test(Bin, vandrs, {10,4,8}, 2),
    correctness_test(Bin, vandrs, {10,4,8}, 3),
    correctness_test(Bin, vandrs, {10,4,8}, 4),

    correctness_test(Bin, cauchyrs, {4,2,3}, 0),
    correctness_test(Bin, cauchyrs, {4,2,3}, 1),
    correctness_test(Bin, cauchyrs, {4,2,3}, 2),

    correctness_test(Bin, liberation, {4,2,7}, 0),
    correctness_test(Bin, liberation, {4,2,7}, 1),
    correctness_test(Bin, liberation, {4,2,7}, 2).


%% @doc Repeat the Encoding Process
%% @private
repeat_encode(_, _, _, _, 0)->
    ok;
repeat_encode(Bin, BinSize, CodingClass, CodingParams, Cnt)->
    io:format("Encode Round Remained: ~p~n", [Cnt]),
    {ok,_} = leo_jerasure:encode(CodingClass, CodingParams, Bin),
    repeat_encode(Bin, BinSize, CodingClass, CodingParams, Cnt - 1).

%% @doc Benchmark Encoding Speed
-spec(benchmark_encode(TotalSizeM, ChunkSizeM, CodingClass, Params) ->
             {ok, atom()} when TotalSizeM::integer(),
                               ChunkSizeM::integer(),
                               CodingClass::atom(),
                               Params::{integer(), integer(), integer()}).
benchmark_encode(TotalSizeM, ChunkSizeM, CodingClass, Params) ->
    KBytes = 1024 * 1024,
    TotalSize = TotalSizeM * KBytes,
    ChunkSize = ChunkSizeM * KBytes,
    ChunkSizeBits = ChunkSize * 8,
    Bin = << 0:ChunkSizeBits>>,
    Start = os:timestamp(),
    repeat_encode(Bin, ChunkSize, CodingClass, Params, (TotalSize div ChunkSize)),
    End = os:timestamp(),
    Time = timer:now_diff(End, Start),
    {ok, Time}.

bench_encode(CodingClass, CodingParams) ->
    {ok, Time} = benchmark_encode(100, 100, CodingClass, CodingParams),
    Rate = 100 / Time * 1000 * 1000,
    ?debugFmt(" * ~p, ~p", [CodingClass, CodingParams]),
    ?debugFmt("   >> Encoding Rate: ~p MB/s", [erlang:round(Rate * 100) / 100]),
    ok.



-endif.
