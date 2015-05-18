#include <string.h>
#include <set>

#include "cauchycoding.h"

#include "jerasure.h"
#include "jerasure_mod.h"
#include "cauchy.h"

void CauchyCoding::checkParams() {
    if (k <= 0 || m <= 0 || w <= 0)
        throw std::invalid_argument("Invalid Coding Parameters");
    if ((k + m) > (1 << w))
        throw std::invalid_argument("Invalid Coding Parameters (larger w)");
}

vector<ERL_NIF_TERM> CauchyCoding::doEncode(ERL_NIF_TERM dataBin) {
    int *matrix = cauchy_good_general_coding_matrix(k, m, w);
    int *bitmatrix = jerasure_matrix_to_bitmatrix(k, m, w, matrix);
    int **smart = jerasure_smart_bitmatrix_to_schedule(k, m, w, bitmatrix);

    char* dataBlocks[k];
    char* codeBlocks[m];

    ErlNifBinary data;
    enif_inspect_binary(env, dataBin, &data);

    size_t dataSize = data.size;
    size_t blockSize = roundTo((roundTo(dataSize, k*w) / (k*w)), 16) * w;

    enif_realloc_binary(&data, blockSize * (k + m));

    for(int i = 0; i < k + m; ++i) {
        (i < k) ? dataBlocks[i] = (char*)data.data + i * blockSize:
            codeBlocks[i - k] = (char*)data.data + i * blockSize;
    }

    jerasure_schedule_encode(k, m, w, smart, dataBlocks, codeBlocks, blockSize, blockSize / w);

    ERL_NIF_TERM allBlocksBin = enif_make_binary(env, &data);

    vector<ERL_NIF_TERM> blockList;
    for(int i = 0 ; i < k + m; ++i) {
        blockList.push_back(enif_make_sub_binary(env, allBlocksBin, i * blockSize, blockSize));
    }

    jerasure_free_schedule(smart);
    free(bitmatrix);
    free(matrix);

    return blockList;
}

ERL_NIF_TERM CauchyCoding::doDecode(vector<ERL_NIF_TERM> blockList, vector<int> blockIdList, size_t dataSize) {

    set<int> availSet(blockIdList.begin(), blockIdList.end());
    if (availSet.size() < (unsigned int)k) 
        throw std::invalid_argument("Not Enough Blocks");
    else if (availSet.size() < blockIdList.size()) {
        throw std::invalid_argument("Blocks should be unique");
    }

    size_t blockSize;

    ErlNifBinary blocks[k + m];
    for(size_t i = 0; i < blockIdList.size(); ++i) {
        int blockId = blockIdList[i];
        enif_inspect_binary(env, blockList[i], &blocks[blockId]);
        blockSize = blocks[blockId].size;
    }

    bool needFix = false;

    for(int i = 0; i < k; ++i) 
        if (availSet.count(i) == 0) {
            needFix = true;
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

    char* dataBlocks[k];
    char* codeBlocks[m];
    int erasures[k + m];
    ErlNifBinary tmpBin;
    enif_alloc_binary(blockSize * (k + m), &tmpBin);
    char* tmpMemory = (char*)tmpBin.data;

    int j = 0;
    for(int i = 0; i < k + m; ++i) {
        i < k ? dataBlocks[i] = tmpMemory + i * blockSize : codeBlocks[i - k] = tmpMemory + i * blockSize;
        if (availSet.count(i) == 0) {
            erasures[j++] = i;
        } else {
            memcpy(tmpMemory + i * blockSize, blocks[i].data, blockSize);
        }
    }
    erasures[j] = -1;

    int *matrix = cauchy_good_general_coding_matrix(k, m, w);
    int *bitmatrix = jerasure_matrix_to_bitmatrix(k, m, w, matrix);
    jerasure_schedule_decode_data_lazy(k, m, w, bitmatrix, erasures, dataBlocks, codeBlocks, blockSize, blockSize / w, 1);

    ERL_NIF_TERM allBlocksBin = enif_make_binary(env, &tmpBin);
    ERL_NIF_TERM bin = enif_make_sub_binary(env, allBlocksBin, 0, dataSize);

    free(matrix);
    free(bitmatrix);
    return bin;
}
