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

typedef enum {
    INVALID_CODING = -1,
    CAUCHY_RS = 1,
    VAND_RS = 2,
	LIBERATION = 3,
} CodingType;

Coding* getCoding(CodingType coding, int k, int m, int w) {
    switch (coding) {
        case CAUCHY_RS:
            return new CauchyCoding(k,m,w);
        case VAND_RS:
            return new RSCoding(k,m,w);
        case LIBERATION:
            return new LiberationCoding(k,m,w);
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
    throw std::invalid_argument("Invalid Coding");
}

vector<ErlNifBinary> doEncode(unsigned char* data, size_t dataSize, int k, int m, int w, CodingType coding) {
    Coding* coder = getCoding(coding, k, m, w);
    coder->checkParams();
    return coder->doEncode(data, dataSize);
} 

ErlNifBinary doDecode(vector<int> availList, vector<ErlNifBinary> blockList, long long fileSize, int k, int m, int w, CodingType coding) {
    Coding* coder = getCoding(coding, k, m, w);
    coder->checkParams();
    return coder->doDecode(blockList, availList, fileSize); 
}

ErlNifBinary doRepair(vector<int> availList, vector<ErlNifBinary> blockList, int repairId, int k, int m, int w, CodingType coding) {
    Coding* coder = getCoding(coding, k, m, w);
    coder->checkParams();
    return coder->doRepair(blockList, availList, repairId); 
}

static ERL_NIF_TERM errTuple(ErlNifEnv *env,const char* message) {
    ERL_NIF_TERM error = enif_make_atom(env, "error");
    ERL_NIF_TERM reason = enif_make_string(env, message, ERL_NIF_LATIN1);
    return enif_make_tuple2(env, error, reason);
}

static ERL_NIF_TERM
encode(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary in;
    if(!enif_inspect_iolist_as_binary(env, argv[0], &in)) {
        return errTuple(env, "Expected Input Bin");
    }
    size_t dataSize = in.size;
    unsigned char* data = in.data;

    char atomString[64];
    if(!enif_get_atom(env, argv[2], atomString, 64, ERL_NIF_LATIN1)) {
        return errTuple(env,"Expect coding");
    }
    
    const ERL_NIF_TERM* tuple;
    int cnt;
    if(!enif_get_tuple(env, argv[3], &cnt, &tuple)) {
        return errTuple(env,"Expect tuple for coding parameters");
    }
    int k,m,w;
    if(!enif_get_int(env, tuple[0], &k))
        return errTuple(env,"Invalid K");
    if(!enif_get_int(env, tuple[1], &m))
        return errTuple(env,"Invalid M");
    if(!enif_get_int(env, tuple[2], &w))
        return errTuple(env,"Invalid W");

    vector<ErlNifBinary> blocks;
    try {
        CodingType coding = getCoding(atomString);
        blocks = doEncode(data, dataSize, k, m, w, coding);
    } catch (std::exception &e) {
        return errTuple(env, e.what());
    }

    ERL_NIF_TERM retArr[blocks.size()];
    for(unsigned int i = 0; i < blocks.size(); ++i) {
        retArr[i] = enif_make_binary(env, &blocks[i]);
    }
    ERL_NIF_TERM blockList = enif_make_list_from_array(env, retArr, blocks.size());
    ERL_NIF_TERM ok = enif_make_atom(env, "ok");
    return enif_make_tuple2(env, ok, blockList);
}

static ERL_NIF_TERM
decode(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

    unsigned int listLen;
    unsigned int listLen2;
    if (!enif_get_list_length(env, argv[0], &listLen)) {
        return errTuple(env,"Block List Needed");
    }
    if (!enif_get_list_length(env, argv[1], &listLen2)) {
        return errTuple(env,"ID List Needed");
    }
    if (listLen != listLen2) {
        return errTuple(env,"Block List and ID List does not match (different Len)");
    }

    ErlNifBinary blockList1[listLen];
    ERL_NIF_TERM BH;
    ERL_NIF_TERM BT;
    int availList1[listLen];
    ERL_NIF_TERM H;
    ERL_NIF_TERM T;

    BT = argv[0];
    T = argv[1];
    for(unsigned int i = 0; i < listLen; ++i) {
        enif_get_list_cell(env, BT, &BH, &BT);
        enif_get_list_cell(env, T, &H, &T);
        ErlNifBinary tmpB;
        if (!enif_inspect_iolist_as_binary(env, BH, &tmpB)) {
            return errTuple(env, "Invalid Block");
        }
        blockList1[i] = tmpB;

        int tmp;
        if (!enif_get_int(env, H, &tmp)) {
            return errTuple(env, "Invalid ID");
        }
        availList1[i] = tmp;
    }

#if SIZEOF_LONG == 8
    unsigned long dataSize;
#else
    uint64_t dataSize;
#endif
    if (!enif_get_uint64(env, argv[2], &dataSize)) {
        return errTuple(env,"Expect data size");
    }

    char atomString[64];
    if(!enif_get_atom(env, argv[3], atomString, 64, ERL_NIF_LATIN1)) {
        return errTuple(env,"Expect coding");
    }
    
    const ERL_NIF_TERM* tuple;
    int cnt;
    if(!enif_get_tuple(env, argv[4], &cnt, &tuple)) {
        return errTuple(env,"Expect tuple for coding parameters");
    }
    int k,m,w;
    if(!enif_get_int(env, tuple[0], &k))
        return errTuple(env,"Invalid K");
    if(!enif_get_int(env, tuple[1], &m))
        return errTuple(env,"Invalid M");
    if(!enif_get_int(env, tuple[2], &w))
        return errTuple(env,"Invalid W");

    vector<ErlNifBinary> blockList2 (blockList1, blockList1 + listLen);
    vector<int> availList2 (availList1, availList1 + listLen);

    ErlNifBinary out;
    try {
        CodingType coding = getCoding(atomString);
        out = doDecode(availList2, blockList2, dataSize, k, m, w, coding);
    } catch (std::exception &e) {
        return errTuple(env,e.what());
    }

    ERL_NIF_TERM bin = enif_make_binary(env, &out);
    ERL_NIF_TERM ok = enif_make_atom(env, "ok");
    return enif_make_tuple2(env, ok, bin);
}

static ERL_NIF_TERM
repair_one(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

    unsigned int listLen;
    unsigned int listLen2;
    if (!enif_get_list_length(env, argv[0], &listLen)) {
        return errTuple(env,"Block List Needed");
    }
    if (!enif_get_list_length(env, argv[1], &listLen2)) {
        return errTuple(env,"ID List Needed");
    }
    if (listLen != listLen2) {
        return errTuple(env,"Block List and ID List does not match (different Len)");
    }

    ErlNifBinary blockList1[listLen];
    ERL_NIF_TERM BH;
    ERL_NIF_TERM BT;
    int availList1[listLen];
    ERL_NIF_TERM H;
    ERL_NIF_TERM T;

    BT = argv[0];
    T = argv[1];
    for(unsigned int i = 0; i < listLen; ++i) {
        enif_get_list_cell(env, BT, &BH, &BT);
        enif_get_list_cell(env, T, &H, &T);
        ErlNifBinary tmpB;
        if (!enif_inspect_iolist_as_binary(env, BH, &tmpB)) {
            return errTuple(env, "Invalid Block");
        }
        blockList1[i] = tmpB;

        int tmp;
        if (!enif_get_int(env, H, &tmp)) {
            return errTuple(env, "Invalid ID");
        }
        availList1[i] = tmp;
    }

    int repairId;
    if (!enif_get_int(env, argv[2], &repairId)) {
        return errTuple(env,"Expect repair ID");
    }

    char atomString[64];
    if(!enif_get_atom(env, argv[3], atomString, 64, ERL_NIF_LATIN1)) {
        return errTuple(env,"Expect coding");
    }
    
    const ERL_NIF_TERM* tuple;
    int cnt;
    if(!enif_get_tuple(env, argv[4], &cnt, &tuple)) {
        return errTuple(env,"Expect tuple for coding parameters");
    }
    int k,m,w;
    if(!enif_get_int(env, tuple[0], &k))
        return errTuple(env,"Invalid K");
    if(!enif_get_int(env, tuple[1], &m))
        return errTuple(env,"Invalid M");
    if(!enif_get_int(env, tuple[2], &w))
        return errTuple(env,"Invalid W");

    vector<ErlNifBinary> blockList2 (blockList1, blockList1 + listLen);
    vector<int> availList2 (availList1, availList1 + listLen);

    ErlNifBinary out;
    try {
        CodingType coding = getCoding(atomString);
        out = doRepair(availList2, blockList2, repairId, k, m, w, coding);
    } catch (std::exception &e) {
        return errTuple(env,e.what());
    }

    ERL_NIF_TERM bin = enif_make_binary(env, &out);
    ERL_NIF_TERM ok = enif_make_atom(env, "ok");
    return enif_make_tuple2(env, ok, bin);
}

static ErlNifFunc nif_funcs[] = {
    {"encode", 4, encode},
    {"decode", 5, decode},
    {"repair_one", 5, repair_one}
};

ERL_NIF_INIT(leo_jerasure, nif_funcs, NULL, NULL, NULL, NULL)
