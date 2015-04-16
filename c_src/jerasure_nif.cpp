#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <vector>
#include <set>
using namespace std;

#include "jerasure.h"
#include "erl_nif.h"

#include "common.h"

#include "cauchy.h"

#include "cauchycoding.h"

typedef enum {
    RS_CAUCHY = 1,
} CodingType;

vector<ErlNifBinary> doEncode(unsigned char* data, size_t dataSize, int k, int m, int w, CodingType coding) {
    CauchyCoding cauchyCoder(k,m,w);
    return cauchyCoder.doEncode(data, dataSize);
} 

ErlNifBinary doDecode(vector<int> availList, vector<ErlNifBinary> blockList, long long fileSize, int k, int m, int w, CodingType coding) {
    CauchyCoding cauchyCoder(k,m,w);
    return cauchyCoder.doDecode(blockList, availList, fileSize); 
}

static ERL_NIF_TERM 
decode_test(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    unsigned int listLen;
    if (!enif_get_list_length(env, argv[0], &listLen)) {
        return enif_make_badarg(env);
    }

    int availList1[listLen];
    ErlNifBinary blockList1[listLen];
//    ERL_NIF_TERM availList[listLen];
    ERL_NIF_TERM H;
    ERL_NIF_TERM T;
    ERL_NIF_TERM BH;
    ERL_NIF_TERM BT;
    T = argv[0];
    BT = argv[1];
    for(unsigned int i = 0; i < listLen; ++i) {
        enif_get_list_cell(env, T, &H, &T);
        enif_get_list_cell(env, BT, &BH, &BT);
        int tmp;
        enif_get_int(env, H, &tmp);
        availList1[i] = tmp;

        ErlNifBinary tmpB;
        enif_inspect_iolist_as_binary(env, BH, &tmpB);
        blockList1[i] = tmpB;
    }
//    if (!enif_get_list_cell(env, argv[0], &availList[0], &availList[listLen - 1])){
//        return enif_make_badarg(env);
//    }

//    ERL_NIF_TERM blockList[listLen];
//    if (!enif_get_list_cell(env, argv[1], &blockList[0], &blockList[listLen - 1])) {
//        return enif_make_badarg(env);
//    }

    int64_t fileSize;
    if (!enif_get_int64(env, argv[2], &fileSize)) {
        return enif_make_badarg(env);
    }


    /*
    for(unsigned int i = 0; i < listLen; ++i) {
        int tmp;
        enif_get_int(env, availList[i], &tmp);
        availList1[i] = tmp;

        ErlNifBinary tmpB;
        enif_inspect_iolist_as_binary(env, blockList[i], &tmpB);
        blockList1[i] = tmpB;
    }
    */

    vector<int> availList2 (availList1, availList1 + listLen);
    vector<ErlNifBinary> blockList2 (blockList1, blockList1 + listLen);

    int k = 10;
    int m = 4;

    ErlNifBinary file = doDecode(availList2, blockList2, fileSize, k, m, 8, RS_CAUCHY);
    return enif_make_binary(env, &file);
}

static ERL_NIF_TERM 
encode_test(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary in;
    if(!enif_inspect_iolist_as_binary(env, argv[0], &in)) {
        return enif_make_badarg(env);
    }
    size_t totalSize = in.size;
    unsigned char* data = in.data;
    int k = 10;
    int m = 4;

    vector<ErlNifBinary> ret = doEncode(data, totalSize, k, m, 8, RS_CAUCHY);
    
    ERL_NIF_TERM retArr[k + m];
//    for(int i = 0; i < k + m; ++i) {
    for(unsigned int i = 0; i < ret.size(); ++i) {
        retArr[i] = enif_make_binary(env, &ret[i]);
    }
    return enif_make_list_from_array(env, retArr, ret.size());
}

static ErlNifFunc nif_funcs[] = {
    {"encode_test", 2, encode_test},
    {"decode_test", 3, decode_test}
};

ERL_NIF_INIT(jerasure, nif_funcs, NULL, NULL, NULL, NULL)
