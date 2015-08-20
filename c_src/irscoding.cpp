#include <string.h>
#include <set>

#include "irscoding.h"

#include "erasure_code.h"

#include "jerasure.h"
#include "jerasure_mod.h"
#include "reed_sol.h"

#include <iostream>
using namespace std;

void IRSCoding::checkParams() {
    if (k <= 0 || m <= 0 || w <= 0)
        throw std::invalid_argument("Invalid Coding Parameters");
	if (w != 8) 
        throw std::invalid_argument("Invalid Coding Parameters (w = 8)");
}

vector<ERL_NIF_TERM> IRSCoding::doEncode(ERL_NIF_TERM dataBin) {
    unsigned char* blocks[k + m];

    ErlNifBinary data;
    enif_inspect_binary(env, dataBin, &data);

    size_t dataSize = data.size;
    size_t blockSize = roundTo((roundTo(dataSize, k*w) / (k*w)), 16) * w;

    size_t offset = 0;
    size_t remain = dataSize;
    int filled = 0;
    while(remain >= blockSize) {
        blocks[filled] = data.data + offset;
        offset += blockSize;
        remain -= blockSize;
        filled++;
    }
    ErlNifBinary tmp;
    enif_alloc_binary((k + m - filled) * blockSize, &tmp);
    memcpy(tmp.data, data.data + offset, remain);
    offset = 0;
    for(int i = filled; i < k + m; ++i, offset += blockSize) {
        blocks[i] = tmp.data + offset;
    }

    uint8_t encode_matrix[(k + m) * k];
    uint8_t g_tbls[k * m * 32];
    gf_gen_cauchy1_matrix(encode_matrix, k + m, k);
    ec_init_tables(k, m, &encode_matrix[k * k], g_tbls);
    ec_encode_data(blockSize, k, m, g_tbls, blocks, &blocks[k]);

    vector<ERL_NIF_TERM> blockList;
    for(int i = 0; i < filled; ++i) {
        blockList.push_back(enif_make_sub_binary(env, dataBin, i * blockSize, blockSize)); 
    }
    ERL_NIF_TERM tmpBin = enif_make_binary(env, &tmp);
    offset = 0;
    for(int i = filled; i < k + m; ++i, offset += blockSize) {
        blockList.push_back(enif_make_sub_binary(env, tmpBin, offset, blockSize));
    }

    return blockList;
}

ERL_NIF_TERM IRSCoding::doDecode(vector<ERL_NIF_TERM> blockList, vector<int> blockIdList, size_t dataSize) {

    set<int> availSet(blockIdList.begin(), blockIdList.end());
    if (availSet.size() < (unsigned int)k) 
        throw std::invalid_argument("Not Enough Blocks");
    else if (availSet.size() < blockIdList.size()) {
        throw std::invalid_argument("Blocks should be unique");
    }

    size_t blockSize;
    unsigned char* availBlocks[k + m];

    ErlNifBinary blocks[k + m];
    for(size_t i = 0; i < blockIdList.size(); ++i) {
        int blockId = blockIdList[i];
        enif_inspect_binary(env, blockList[i], &blocks[blockId]);
        blockSize = blocks[blockId].size;
        availBlocks[i] = blocks[blockId].data;
    }

    bool needFix = false;
    int dataErasures = 0;
    vector<int> outBlockIdList;
    ErlNifBinary tmpBin;
    enif_alloc_binary(blockSize * k, &tmpBin);
    unsigned char* outBlocks[dataErasures];

    for(int i = 0; i < k; ++i) 
        if (availSet.count(i) == 0) {
            needFix = true;
            outBlockIdList.push_back(i);
            outBlocks[dataErasures] = tmpBin.data + i * blockSize;
            ++dataErasures;
        } else {
            memcpy(tmpBin.data + i * blockSize, blocks[i].data, blockSize);
        }

    if (!needFix) {
        ErlNifBinary file;
        enif_alloc_binary(dataSize, &file);
        size_t copySize, offset = 0;
        for(int i = 0; i < k; ++i) {
            offset = i * blockSize;
            copySize = min(dataSize - offset, blockSize);
            memcpy(file.data + offset, blocks[i].data, copySize);
        }
        ERL_NIF_TERM bin = enif_make_binary(env, &file);
        return bin;
    }

    uint8_t encode_matrix[(k + m) * k];
    uint8_t g_tbls[k * m * 32];
    uint8_t decode_matrix[(k + m) * k];
    gf_gen_cauchy1_matrix(encode_matrix, k + m, k);
    gf_gen_decode_matrix(encode_matrix, decode_matrix, blockIdList, outBlockIdList);
    ec_init_tables(k, dataErasures, decode_matrix, g_tbls);
    ec_encode_data(blockSize, k, dataErasures, g_tbls, availBlocks, outBlocks);

    ERL_NIF_TERM allBlocksBin = enif_make_binary(env, &tmpBin);
    ERL_NIF_TERM bin = enif_make_sub_binary(env, allBlocksBin, 0, dataSize);
    return bin;
}

vector<ERL_NIF_TERM> IRSCoding::doRepair(vector<ERL_NIF_TERM> blockList, vector<int> blockIdList, vector<int> repairList) {

    set<int> availSet(blockIdList.begin(), blockIdList.end());
    if (availSet.size() < (unsigned int)k) 
        throw std::invalid_argument("Not Enough Blocks");
    else if (availSet.size() < blockIdList.size()) {
        throw std::invalid_argument("Blocks should be unique");
    }

    size_t blockSize;
    unsigned char* availBlocks[k + m];

    ErlNifBinary blocks[k + m];
    for(size_t i = 0; i < blockIdList.size(); ++i) {
        int blockId = blockIdList[i];
        enif_inspect_binary(env, blockList[i], &blocks[blockId]);
        blockSize = blocks[blockId].size;
        availBlocks[i] = blocks[blockId].data;
    }

    int outSize = repairList.size();
    ErlNifBinary tmpBin;
    enif_alloc_binary(blockSize * k, &tmpBin);
    unsigned char* outBlocks[outSize];

    for(int i = 0; i < outSize; ++i) {
        outBlocks[i] = tmpBin.data + i * blockSize;
    }

    uint8_t encode_matrix[(k + m) * k];
    uint8_t g_tbls[k * m * 32];
    uint8_t decode_matrix[(k + m) * k];
    gf_gen_cauchy1_matrix(encode_matrix, k + m, k);
    gf_gen_decode_matrix(encode_matrix, decode_matrix, blockIdList, repairList);
    ec_init_tables(k, outSize, decode_matrix, g_tbls);
    ec_encode_data(blockSize, k, outSize, g_tbls, availBlocks, outBlocks);
    
    vector<ERL_NIF_TERM> repairBlocks;
    ERL_NIF_TERM outBlockBin = enif_make_binary(env, &tmpBin);
    for(size_t i = 0; i < repairList.size(); ++i){
        ERL_NIF_TERM block = enif_make_sub_binary(env, outBlockBin, i * blockSize, blockSize); 
        repairBlocks.push_back(block);
    }

    return repairBlocks; 
}

void IRSCoding::gf_gen_decode_matrix(
        unsigned char* encode_matrix,
        unsigned char* decode_matrix,
        vector<int> availBlockIdList,
        vector<int> outBlockIdList) {
    unsigned char invert_matrix[(k + m) * k];
    unsigned char temp_matrix[(k + m) * k];
    for(int i = 0; i < k; ++i){
        int blockId = availBlockIdList[i];
        for(int j = 0; j < k; ++j) {
            temp_matrix[i * k + j] = encode_matrix[blockId * k + j];
        }
    }
    if (gf_invert_matrix(temp_matrix, invert_matrix, k) < 0) 
        throw std::invalid_argument("Non Invertible");
    for(size_t i = 0; i < outBlockIdList.size(); ++i) {
        int blockId = outBlockIdList[i];
        if (blockId < k) {
            for(int j = 0; j < k; ++j) {
                decode_matrix[k * i + j] = invert_matrix[k * blockId + j];
            }
        } else {
            for(int j = 0; j < k; ++j) {
                unsigned char s = 0;
                for(int l = 0; l < k; ++l) {
                    s ^= gf_mul(invert_matrix[l * k + j],
                            encode_matrix[k * blockId + l]);
                }
                decode_matrix[k * i + j] = s;
            }
        }
    } 
}
