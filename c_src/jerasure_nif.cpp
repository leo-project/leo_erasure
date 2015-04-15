#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <vector>
#include <set>
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

ErlNifBinary doDecode(vector<int> availList, vector<ErlNifBinary> blockList, long long fileSize, int k, int m, int w, CodingType coding) {
    ErlNifBinary file;
    char** dataBlocks = (char**)enif_alloc(sizeof(char*) * k);
    char** codeBlocks = (char**)enif_alloc(sizeof(char*) * m);
    int erasures[k + m];
    size_t blockSize = blockList[0].size;
    set<int> availSet(availList.begin(), availList.end());

    int j = 0;
    for(int i = 0; i < k + m; ++i) {
        i < k ? dataBlocks[i] = (char*)enif_alloc(blockSize) : codeBlocks[i - k] = 
                        (char*)enif_alloc(blockSize);
        if (availSet.count(i) == 0) {
            erasures[j++] = i;
        }
    }
    erasures[j] = -1;


    for(unsigned int i = 0; i < blockList.size(); ++i) {
        int blockId = availList[i];
        if ((int)blockId < k) {
            memcpy(dataBlocks[blockId], blockList[i].data, blockSize);
        } else {
            memcpy(codeBlocks[blockId - k], blockList[i].data, blockSize);
        }
    }

    int *matrix = cauchy_good_general_coding_matrix(k, m, w);
    int *bitmatrix = jerasure_matrix_to_bitmatrix(k, m, w, matrix);
    jerasure_schedule_decode_lazy(k, m, w, bitmatrix, erasures, dataBlocks, codeBlocks, blockSize, blockSize / w, 1);

    enif_alloc_binary(fileSize, &file);
    size_t offset = 0;
    for(int i = 0; i < k - 1; ++i) {
        memcpy(file.data + offset, dataBlocks[i], blockSize);
        offset += blockSize;
    }
    memcpy(file.data + offset, dataBlocks[k - 1], fileSize - offset);

    for(int i = 0; i < k + m; ++i) {
        i < k ? enif_free(dataBlocks[i]) : enif_free(codeBlocks[i - k]);
    }
    enif_free(dataBlocks);
    enif_free(codeBlocks);
    return file;
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

    int k = 4;
    int m = 2;

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
    int k = 4;
    int m = 2;

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
