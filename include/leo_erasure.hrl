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

-define(CODING_CLASS_VANDRS,     vandrs).
-define(CODING_CLASS_CAUCHYRS,   cauchyrs).
-define(CODING_CLASS_LIBERATION, liberation).
-type(coding_class() :: ?CODING_CLASS_VANDRS |
                        ?CODING_CLASS_CAUCHYRS |
                        ?CODING_CLASS_LIBERATION).

-define(DEF_CODING_CLASS, ?CODING_CLASS_VANDRS).
-define(DEF_CODING_PARAMS, {10, 4, 8}).

-type(coding_params() :: {pos_integer(), pos_integer(), pos_integer()}).
-type(id_with_block() :: {non_neg_integer(), binary()}).

-define(DEF_CODING_VANDRS_K,    10).
-define(DEF_CODING_CAUCHYRS_K,   4).
-define(DEF_CODING_LIBERATION_K, 4).

-define(DEF_CODING_VANDRS_M,     4).
-define(DEF_CODING_CAUCHYRS_M,   2).
-define(DEF_CODING_LIBERATION_M, 2).

-define(DEF_CODING_VANDRS_W,     8).
-define(DEF_CODING_CAUCHYRS_W,   3).
-define(DEF_CODING_LIBERATION_W, 7).

-define(coding_params_k(_CodingClass),
        case _CodingClass of
            ?CODING_CLASS_VANDRS ->
                ?DEF_CODING_VANDRS_K;
            ?CODING_CLASS_CAUCHYRS ->
                ?DEF_CODING_CAUCHYRS_K;
            ?CODING_CLASS_LIBERATION ->
                ?DEF_CODING_LIBERATION_K
        end).
-define(coding_params_m(_CodingClass),
        case _CodingClass of
            ?CODING_CLASS_VANDRS ->
                ?DEF_CODING_VANDRS_M;
            ?CODING_CLASS_CAUCHYRS ->
                ?DEF_CODING_CAUCHYRS_M;
            ?CODING_CLASS_LIBERATION ->
                ?DEF_CODING_LIBERATION_M
        end).
-define(coding_params_w(_CodingClass),
        case _CodingClass of
            ?CODING_CLASS_VANDRS ->
                ?DEF_CODING_VANDRS_W;
            ?CODING_CLASS_CAUCHYRS ->
                ?DEF_CODING_CAUCHYRS_W;
            ?CODING_CLASS_LIBERATION ->
                ?DEF_CODING_LIBERATION_W
        end).
