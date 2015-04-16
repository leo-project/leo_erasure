#include <stdio.h>
#include <stdint.h>
#include <vector>
#include <string>
using namespace std;

#include "erl_nif.h"

#include "cauchycoding.h"

typedef enum {
    INVALID_CODING = -1,
    CAUCHY_RS = 1,
} CodingType;

vector<ErlNifBinary> doEncode(unsigned char* data, size_t dataSize, int k, int m, int w, CodingType coding) {
    Coding* coder;
    switch (coding) {
        case CAUCHY_RS:
            coder = new CauchyCoding(k,m,w);
            break;
        default:
            break;
    }
    return coder->doEncode(data, dataSize);
} 

ErlNifBinary doDecode(vector<int> availList, vector<ErlNifBinary> blockList, long long fileSize, int k, int m, int w, CodingType coding) {
    Coding* coder;
    switch (coding) {
        case CAUCHY_RS:
            coder = new CauchyCoding(k,m,w);
            break;
        default:
            break;
    }
    return coder->doDecode(blockList, availList, fileSize); 
}

CodingType getCoding(char* codingAtom) {
    string atomString(codingAtom);
    if (atomString == "cauchyrs")
        return CAUCHY_RS;
    return INVALID_CODING;
}

static ERL_NIF_TERM
encode(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary in;
    if(!enif_inspect_iolist_as_binary(env, argv[0], &in)) {
        return enif_make_badarg(env);
    }
    size_t dataSize = in.size;
    unsigned char* data = in.data;

    char atomString[64];
    if(!enif_get_atom(env, argv[2], atomString, 64, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }
    
    CodingType coding = getCoding(atomString);
    /// TODO: Error Detection

    const ERL_NIF_TERM* tuple;
    int cnt;
    if(!enif_get_tuple(env, argv[3], &cnt, &tuple)) {
        return enif_make_badarg(env);
    }
    int k,m,w;
    enif_get_int(env, tuple[0], &k);
    enif_get_int(env, tuple[1], &m);
    enif_get_int(env, tuple[2], &w);

    vector<ErlNifBinary> blocks = doEncode(data, dataSize, k, m, w, coding);

    ERL_NIF_TERM retArr[blocks.size()];
    for(unsigned int i = 0; i < blocks.size(); ++i) {
        retArr[i] = enif_make_binary(env, &blocks[i]);
    }
    return enif_make_list_from_array(env, retArr, blocks.size());
}

static ERL_NIF_TERM
decode(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    unsigned int listLen;
    if (!enif_get_list_length(env, argv[0], &listLen)) {
        return enif_make_badarg(env);
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
        enif_inspect_iolist_as_binary(env, BH, &tmpB);
        blockList1[i] = tmpB;

        int tmp;
        enif_get_int(env, H, &tmp);
        availList1[i] = tmp;
    }

    uint64_t dataSize;
    if (!enif_get_uint64(env, argv[2], &dataSize)) {
        return enif_make_badarg(env);
    }

    char atomString[64];
    if(!enif_get_atom(env, argv[3], atomString, 64, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }
    
    CodingType coding = getCoding(atomString);
    /// TODO: Error Detection

    const ERL_NIF_TERM* tuple;
    int cnt;
    if(!enif_get_tuple(env, argv[4], &cnt, &tuple)) {
        return enif_make_badarg(env);
    }
    int k,m,w;
    enif_get_int(env, tuple[0], &k);
    enif_get_int(env, tuple[1], &m);
    enif_get_int(env, tuple[2], &w);

    vector<ErlNifBinary> blockList2 (blockList1, blockList1 + listLen);
    vector<int> availList2 (availList1, availList1 + listLen);

    ErlNifBinary out = doDecode(availList2, blockList2, dataSize, k, m, w, coding);
    return enif_make_binary(env, &out);
}

static ErlNifFunc nif_funcs[] = {
    {"encode", 4, encode},
    {"decode", 5, decode},
};

ERL_NIF_INIT(jerasure, nif_funcs, NULL, NULL, NULL, NULL)
