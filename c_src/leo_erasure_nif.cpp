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
#include <stdio.h>
#include <stdint.h>
#include <vector>
#include <string>
#include <stdexcept>
using namespace std;

#include "erl_nif.h"
#include "liberationcoding.h"
#include "cauchycoding.h"
#include "rscoding.h"
#include "irscoding.h"
#include "galois.h"

typedef enum {
    INVALID_CODING = -1,
    CAUCHY_RS = 1,
    VAND_RS = 2,
	LIBERATION = 3,
    ISA_RS = 4,
} CodingType;

Coding* getCoder(CodingType coding, int k, int m, int w, ErlNifEnv* env) {
    switch (coding) {
        case CAUCHY_RS:
            return new CauchyCoding(k,m,w,env);
        case VAND_RS:
            return new RSCoding(k,m,w,env);
        case LIBERATION:
            return new LiberationCoding(k,m,w,env);
        case ISA_RS:
            return new IRSCoding(k,m,w,env);
        default:
            throw std::invalid_argument("Invalid Coding");
            break;
    }
    return NULL;
}

CodingType getCoding(char* codingAtom) {
    string atomString(codingAtom);
    if (atomString == "cauchyrs")
        return CAUCHY_RS;
    if (atomString == "vandrs")
        return VAND_RS;
    if (atomString == "liberation")
        return LIBERATION;
    if (atomString == "isars")
        return ISA_RS;
    throw std::invalid_argument("Invalid Coding");
}

vector<ERL_NIF_TERM> doEncode(ERL_NIF_TERM data, CodingType coding, int k, int m, int w, ErlNifEnv* env) {
    Coding* coder = getCoder(coding, k, m, w, env);
    vector<ERL_NIF_TERM> ret;
    try {
        coder->checkParams();
        ret = coder->doEncode(data);
    } catch (std::exception &e) {
        delete coder;
        throw e;
    }
    delete coder;
    return ret;
}

ERL_NIF_TERM doDecode(vector<ERL_NIF_TERM> blockList, vector<int> idList, size_t fileSize, CodingType coding, int k, int m, int w, ErlNifEnv* env) {
    Coding* coder = getCoder(coding, k, m, w, env);
    ERL_NIF_TERM ret;
    try {
        coder->checkParams();
        ret = coder->doDecode(blockList, idList, fileSize);
    } catch (std::exception &e) {
        delete coder;
        throw e;
    }
    delete coder;
    return ret;
}

vector<ERL_NIF_TERM> doRepair(vector<ERL_NIF_TERM> blockList, vector<int> idList, vector<int> repairList, CodingType coding, int k, int m, int w, ErlNifEnv* env) {
    Coding* coder = getCoder(coding, k, m, w, env);
    vector<ERL_NIF_TERM> ret;
    try {
        coder->checkParams();
        ret = coder->doRepair(blockList, idList, repairList);
    } catch (std::exception &e) {
        delete coder;
        throw e;
    }
    delete coder;
    return ret;
}

static ERL_NIF_TERM errTuple(ErlNifEnv *env,const char* message) {
    ERL_NIF_TERM error = enif_make_atom(env, "error");
    ERL_NIF_TERM reason = enif_make_string(env, message, ERL_NIF_LATIN1);
    return enif_make_tuple2(env, error, reason);
}

ERL_NIF_TERM
gf_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (galois_init_default_field_noalloc(8)) return errTuple(env, "Galois Initialization Failed! w=8");
    if (galois_init_default_field_noalloc(16)) return errTuple(env, "Galois Initialization Failed! w=16");
    if (galois_init_default_field_noalloc(32)) return errTuple(env, "Galois Initialization Failed! w=32");
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
encode(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary in;
    if(!enif_inspect_iolist_as_binary(env, argv[2], &in)) {
        return errTuple(env, "Expected Input Bin");
    }

    char atomString[64];
    if(!enif_get_atom(env, argv[0], atomString, 64, ERL_NIF_LATIN1)) {
        return errTuple(env,"Expect coding");
    }

    const ERL_NIF_TERM* tuple;
    int cnt;
    if(!enif_get_tuple(env, argv[1], &cnt, &tuple)) {
        return errTuple(env,"Expect tuple for coding parameters");
    }
    int k,m,w;
    if(!enif_get_int(env, tuple[0], &k))
        return errTuple(env,"Invalid K");
    if(!enif_get_int(env, tuple[1], &m))
        return errTuple(env,"Invalid M");
    if(!enif_get_int(env, tuple[2], &w))
        return errTuple(env,"Invalid W");

    vector<ERL_NIF_TERM> blocks;
    ERL_NIF_TERM blockList;
    try {
        CodingType coding = getCoding(atomString);
        blocks = doEncode(argv[2], coding, k, m, w, env);
    } catch (std::exception &e) {
        return errTuple(env, e.what());
    }
    blockList = enif_make_list_from_array(env, &blocks[0], blocks.size());
    ERL_NIF_TERM ok = enif_make_atom(env, "ok");
    return enif_make_tuple2(env, ok, blockList);
}


static ERL_NIF_TERM
decode(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    unsigned int listLen;
    unsigned int listLen2;
    if (!enif_get_list_length(env, argv[2], &listLen)) {
        return errTuple(env,"Block List Needed");
    }
    if (!enif_get_list_length(env, argv[3], &listLen2)) {
        return errTuple(env,"ID List Needed");
    }
    if (listLen != listLen2) {
        return errTuple(env,"Block List and ID List does not match (different Len)");
    }

    vector<ERL_NIF_TERM> blockList;
    blockList.resize(listLen);
    ERL_NIF_TERM BH;
    ERL_NIF_TERM BT;

    vector<int> idList;
    idList.resize(listLen);
    ERL_NIF_TERM H;
    ERL_NIF_TERM T;

    BT = argv[2];
    T = argv[3];
    for(unsigned int i = 0; i < listLen; ++i) {
        enif_get_list_cell(env, BT, &BH, &BT);
        enif_get_list_cell(env, T, &H, &T);

        ErlNifBinary tmpB;
        if (!enif_inspect_iolist_as_binary(env, BH, &tmpB)) {
            return errTuple(env, "Invalid Block");
        }
        blockList[i] = BH;

        int tmp;
        if (!enif_get_int(env, H, &tmp)) {
            return errTuple(env, "Invalid ID");
        }
        idList[i] = tmp;
    }

#if SIZEOF_LONG == 8
    unsigned long dataSize;
#else
    uint64_t dataSize;
#endif
    if (!enif_get_uint64(env, argv[4], &dataSize)) {
        return errTuple(env,"Expect data size");
    }

    char atomString[64];
    if(!enif_get_atom(env, argv[0], atomString, 64, ERL_NIF_LATIN1)) {
        return errTuple(env,"Expect coding");
    }

    const ERL_NIF_TERM* tuple;
    int cnt;
    if(!enif_get_tuple(env, argv[1], &cnt, &tuple)) {
        return errTuple(env,"Expect tuple for coding parameters");
    }
    int k,m,w;
    if(!enif_get_int(env, tuple[0], &k))
        return errTuple(env,"Invalid K");
    if(!enif_get_int(env, tuple[1], &m))
        return errTuple(env,"Invalid M");
    if(!enif_get_int(env, tuple[2], &w))
        return errTuple(env,"Invalid W");

    ERL_NIF_TERM bin;
    try {
        CodingType coding = getCoding(atomString);
        bin = doDecode(blockList, idList, dataSize, coding, k, m, w, env);
    } catch (std::exception &e) {
        return errTuple(env,e.what());
    }

    ERL_NIF_TERM ok = enif_make_atom(env, "ok");
    return enif_make_tuple2(env, ok, bin);
}


static ERL_NIF_TERM
repair(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

    unsigned int listLen;
    unsigned int listLen2;
    if (!enif_get_list_length(env, argv[2], &listLen)) {
        return errTuple(env,"Block List Needed");
    }
    if (!enif_get_list_length(env, argv[3], &listLen2)) {
        return errTuple(env,"ID List Needed");
    }
    if (listLen != listLen2) {
        return errTuple(env,"Block List and ID List does not match (different Len)");
    }

    vector<ERL_NIF_TERM> blockList;
    blockList.resize(listLen);
    ERL_NIF_TERM BH;
    ERL_NIF_TERM BT;

    vector<int> idList;
    idList.resize(listLen);
    ERL_NIF_TERM H;
    ERL_NIF_TERM T;

    BT = argv[2];
    T = argv[3];
    for(unsigned int i = 0; i < listLen; ++i) {
        enif_get_list_cell(env, BT, &BH, &BT);
        enif_get_list_cell(env, T, &H, &T);

        ErlNifBinary tmpB;
        if (!enif_inspect_iolist_as_binary(env, BH, &tmpB)) {
            return errTuple(env, "Invalid Block");
        }
        blockList[i] = BH;

        int tmp;
        if (!enif_get_int(env, H, &tmp)) {
            return errTuple(env, "Invalid ID");
        }
        idList[i] = tmp;
    }

    unsigned int listLen3;
    if (!enif_get_list_length(env, argv[4], &listLen3)) {
        return errTuple(env,"Repair ID List Needed");
    }
    vector<int> repairList;
    repairList.resize(listLen3);
    ERL_NIF_TERM RH;
    ERL_NIF_TERM RT;
    RT = argv[4];
    for(unsigned int i = 0; i < listLen3; ++i) {
        enif_get_list_cell(env, RT, &RH, &RT);
        int repairId;
        if (!enif_get_int(env, RH, &repairId)) {
            return errTuple(env, "Invalid Repair ID");
        }
        repairList[i] = repairId;
    }

    char atomString[64];
    if(!enif_get_atom(env, argv[0], atomString, 64, ERL_NIF_LATIN1)) {
        return errTuple(env,"Expect coding");
    }

    const ERL_NIF_TERM* tuple;
    int cnt;
    if(!enif_get_tuple(env, argv[1], &cnt, &tuple)) {
        return errTuple(env,"Expect tuple for coding parameters");
    }
    int k,m,w;
    if(!enif_get_int(env, tuple[0], &k))
        return errTuple(env,"Invalid K");
    if(!enif_get_int(env, tuple[1], &m))
        return errTuple(env,"Invalid M");
    if(!enif_get_int(env, tuple[2], &w))
        return errTuple(env,"Invalid W");

    vector<ERL_NIF_TERM> blocks;
    ERL_NIF_TERM blockListR;
    try {
        CodingType coding = getCoding(atomString);
        blocks = doRepair(blockList, idList, repairList, coding, k, m, w, env);
    } catch (std::exception &e) {
        return errTuple(env,e.what());
    }

    blockListR = enif_make_list_from_array(env, &blocks[0], blocks.size());
    ERL_NIF_TERM ok = enif_make_atom(env, "ok");
    return enif_make_tuple2(env, ok, blockListR);
}

static ErlNifFunc nif_funcs[] = {
    {"gf_init",0, gf_init},
    {"encode", 4, encode},
    {"decode", 5, decode},
    {"repair", 5, repair}
};

ERL_NIF_INIT(leo_erasure, nif_funcs, NULL, NULL, NULL, NULL)
