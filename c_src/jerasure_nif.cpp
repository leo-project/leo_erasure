#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
using namespace std;

#include "jerasure.h"
#include "erl_nif.h"

#include "cauchy.h"

typedef enum {
    RS_CAUCHY = 1,
} CodingType;

typedef struct {
    size_t size;
    unsigned char* data;
} BlockEntry;

uint32_t roundTo(uint32_t numToRound, uint32_t multiple) {
    if (multiple == 0) {
        return numToRound;
    }

    uint32_t remainder = numToRound % multiple;
    if (remainder == 0)
        return numToRound;
    return numToRound + multiple - remainder;
}

vector<ErlNifBinary> doEncode(unsigned char* data, size_t dataSize, int k, int m, int w, CodingType coding) {
    vector<ErlNifBinary> allBlockBinary;
    int *matrix = cauchy_good_general_coding_matrix(k, m, w);
    int *bitmatrix = jerasure_matrix_to_bitmatrix(k, m, w, matrix);
    int **smart = jerasure_smart_bitmatrix_to_schedule(k, m, w, bitmatrix);

    long long blockSize = roundTo((roundTo(dataSize, k*w) / (k*w)), 4) * w;


    char** dataBlocks = (char**)enif_alloc(sizeof(char*) * k);
    for(int i = 0; i < k - 1; ++i) {
        ErlNifBinary dataBlock;
        enif_alloc_binary(blockSize, &dataBlock);
        memcpy(dataBlock.data, data + blockSize * i, blockSize);
        allBlockBinary.push_back(dataBlock);
        dataBlocks[i] = (char*)dataBlock.data;
    }

    // Padding Last Block
    ErlNifBinary lastBlock;
    enif_alloc_binary(blockSize, &lastBlock);
    memset(lastBlock.data, 0, blockSize);
    printf("Data Size: %d , Blocks Size: %d (%d,%d)\n", dataSize, blockSize, k, m);
    printf("Copy Size: %d\n", dataSize - (blockSize * (k - 1)));
    memcpy(lastBlock.data, data + blockSize * (k - 1), dataSize - (blockSize * (k - 1)));
    allBlockBinary.push_back(lastBlock);
    dataBlocks[k - 1] = (char*)lastBlock.data;

    char** codeBlocks = (char**)enif_alloc(sizeof(char*) * m);
    for(int i = 0; i < m; ++i) {
        ErlNifBinary codeBlock;
        enif_alloc_binary(blockSize, &codeBlock);
        allBlockBinary.push_back(codeBlock);
        codeBlocks[i] = (char*)codeBlock.data;
    }

    jerasure_schedule_encode(k, m, w, smart, dataBlocks, codeBlocks, blockSize, blockSize / w);

    enif_free(dataBlocks);
    enif_free(codeBlocks);
    return allBlockBinary;
} 

static ERL_NIF_TERM 
decode_test(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
}

static ERL_NIF_TERM 
encode_test(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary in;
    if(!enif_inspect_iolist_as_binary(env, argv[0], &in)) {
        return enif_make_badarg(env);
    }
    size_t totalSize = in.size;
    unsigned char* data = in.data;
    int k = 4;
    int m = 2;
    vector<ErlNifBinary> ret = doEncode(data, totalSize, k, m, 8, RS_CAUCHY);
    
    ERL_NIF_TERM retArr[k + m];
//    for(int i = 0; i < k + m; ++i) {
    for(int i = 0; i < ret.size(); ++i) {
        retArr[i] = enif_make_binary(env, &ret[i]);
    }
    return enif_make_list_from_array(env, retArr, ret.size());
}

static ErlNifFunc nif_funcs[] = {
    {"encode_test", 2, encode_test},
    {"decode_test", 2, decode_test}
};

ERL_NIF_INIT(jerasure, nif_funcs, NULL, NULL, NULL, NULL)
