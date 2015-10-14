// -------------------------------------------------------------------
//
// leo_erasure: Erasure code library for Erlang
//
// Copyright (c) 2012-2015 Rakuten, Inc.
//
// This file is provided to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file
// except in compliance with the License.  You may obtain
// a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.
//
// -------------------------------------------------------------------
#include "coding.h"

vector<ERL_NIF_TERM> Coding::doRepair(vector<ERL_NIF_TERM> blockList, vector<int> blockIdList, vector<int> repairList) {
    ErlNifBinary bin;
    enif_inspect_binary(env, blockList[0], &bin);
    ERL_NIF_TERM file = doDecode(blockList, blockIdList, k * bin.size);
    vector<ERL_NIF_TERM> blocks = doEncode(file);

    vector<ERL_NIF_TERM> repairBlocks;
    for (vector<int>::iterator it = repairList.begin(); it != repairList.end(); ++it) {
        repairBlocks.push_back(blocks[*it]);
    }
    return repairBlocks;
}
