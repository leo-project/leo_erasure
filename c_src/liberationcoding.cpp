#include <string.h>
#include <set>

#include "liberationcoding.h"
#include "alloc.h"

#include "jerasure.h"
#include "liberation.h"

void LiberationCoding::checkParams() {

}

vector<ErlNifBinary> LiberationCoding::doEncode(unsigned char* data, size_t dataSize) {
    vector<ErlNifBinary> allBlockEntry;

    int *bitmatrix = liberation_coding_bitmatrix(k, w);
    int **smart = jerasure_smart_bitmatrix_to_schedule(k, m, w, bitmatrix);

    size_t blockSize = roundTo((roundTo(dataSize, k*w) / (k*w)), sizeof(long)) * w;

    char** dataBlocks = (char**)alloc(sizeof(char*) * k);
    char** codeBlocks = (char**)alloc(sizeof(char*) * m);

    size_t offset = 0;
    for(int i = 0; i < k; ++i) {
        ErlNifBinary tmpBlock;
        enif_alloc_binary(blockSize, &tmpBlock);
        dataBlocks[i] = (char*)tmpBlock.data;
        
        allBlockEntry.push_back(tmpBlock);

        if (i == k - 1) {
            memset(dataBlocks[i], 0, blockSize);
            memcpy(dataBlocks[i], data + offset, dataSize - offset);
        } else {
            memcpy(dataBlocks[i], data + offset, blockSize);
        }
        offset += blockSize;
    }

    for(int i = 0; i < m; ++i) {
        ErlNifBinary tmpBlock;
        enif_alloc_binary(blockSize, &tmpBlock);
        codeBlocks[i] = (char*)tmpBlock.data;
        
        allBlockEntry.push_back(tmpBlock);
    }

    jerasure_schedule_encode(k, m, w, smart, dataBlocks, codeBlocks, blockSize, blockSize / w);

    jerasure_free_schedule(smart);
    free(bitmatrix);
    dealloc(dataBlocks);
    dealloc(codeBlocks);
    return allBlockEntry;
}

ErlNifBinary LiberationCoding::doDecode(vector<ErlNifBinary> blockList, vector<int> blockIdList, size_t dataSize) {

    int *bitmatrix = liberation_coding_bitmatrix(k, w);

    ErlNifBinary file;

    size_t blockSize = blockList[0].size;
    set<int> availSet(blockIdList.begin(), blockIdList.end());

    bool needFix = false;
    for(int i = 0; i < k; ++i) 
        if (availSet.count(i) == 0) {
            needFix = true;
            break;
        }

    if (!needFix) {
        enif_alloc_binary(dataSize, &file);
        size_t offset = 0;
        int i = 0;
        while(offset < dataSize) {
            size_t copySize = min(dataSize - offset, blockSize);
            memcpy(file.data + offset, blockList[i].data, copySize);
            i++;
            offset += copySize;
        }
        
        return file;
    }

    char** dataBlocks = (char**)alloc(sizeof(char*) * k);
    char** codeBlocks = (char**)alloc(sizeof(char*) * m);
    int erasures[k + m];
    int j = 0;
    for(int i = 0; i < k + m; ++i) {
        i < k ? dataBlocks[i] = (char*)alloc(blockSize) : codeBlocks[i - k] = (char*)alloc(blockSize);
        if (availSet.count(i) == 0) {
            erasures[j++] = i;
        }
    }
    erasures[j] = -1;

    for(size_t i = 0; i < blockList.size(); ++i) {
        int blockId = blockIdList[i];
        if (blockId < k) {
            memcpy(dataBlocks[blockId], blockList[i].data, blockSize);
        } else {
            memcpy(codeBlocks[blockId - k], blockList[i].data, blockSize);
        }
    }

    jerasure_schedule_decode_lazy(k, m, w, bitmatrix, erasures, dataBlocks, codeBlocks, blockSize, blockSize / w, 0);

    enif_alloc_binary(dataSize, &file);
    size_t offset = 0;
    int i = 0;
    while(offset < dataSize) {
        size_t copySize = min(dataSize - offset, blockSize);
        memcpy(file.data + offset, dataBlocks[i], copySize);
        i++;
        offset += copySize;
    }

    for(int i = 0; i < k + m; ++i) {
        i < k ? dealloc(dataBlocks[i]) : dealloc(codeBlocks[i - k]);
    }

    free(bitmatrix);
    dealloc(dataBlocks);
    dealloc(codeBlocks);
    return file;
}
