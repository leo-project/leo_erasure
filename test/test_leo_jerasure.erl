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

bench_encode_test() ->
    {ok, _} = leo_jerasure:benchmark_encode(100,100,vandrs,{4,2,8}),
    {ok, _} = leo_jerasure:benchmark_encode(100,100,cauchyrs,{4,2,3}),
    {ok, _} = leo_jerasure:benchmark_encode(100,100,liberation,{4,2,7}).

-endif.
