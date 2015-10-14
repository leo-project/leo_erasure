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
#ifndef __CODING_H__
#define __CODING_H__

#include <vector>
#include <stdexcept>
using namespace std;

#include "common.h"
#include "erl_nif.h"

class Coding {
    public:
        Coding(int _k, int _m, int _w, ErlNifEnv* _env) : k(_k), m(_m), w(_w), env(_env) {};

        virtual vector<ERL_NIF_TERM> doEncode(ERL_NIF_TERM dataBin) = 0;
        virtual ERL_NIF_TERM doDecode(vector<ERL_NIF_TERM> blockList, vector<int> blockIdList, size_t dataSize) = 0;
        virtual vector<ERL_NIF_TERM> doRepair(vector<ERL_NIF_TERM> blockList, vector<int> blockIdList, vector<int> repairList);
        virtual void checkParams() = 0;
    protected:
        int k, m, w;
        ErlNifEnv* env;
};

#endif
