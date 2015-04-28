#include <string.h>
#include <set>

#include "rscoding.h"
#include "alloc.h"

#include "jerasure.h"
#include "reed_sol.h"

void RSCoding::checkParams() {
    if (k <= 0 || m <= 0 || w <= 0)
        throw std::invalid_argument("Invalid Coding Parameters");
	if (w != 8 && w != 16 && w != 32) 
        throw std::invalid_argument("Invalid Coding Parameters (w = 8/16/32)");
}

vector<ErlNifBinary> RSCoding::doEncode(unsigned char* data, size_t dataSize) {
    vector<ErlNifBinary> allBlockEntry;

    int *matrix = reed_sol_vandermonde_coding_matrix(k, m, w);

    size_t blockSize = roundTo((roundTo(dataSize, k*w) / (k*w)), 16) * w;

    char** dataBlocks = (char**)alloc(sizeof(char*) * k);
    char** codeBlocks = (char**)alloc(sizeof(char*) * m);

    unsigned int align;
    bool aligned = true;
    size_t offset = 0;

    for(int i = 0; i < k; ++i) {
        ErlNifBinary tmpBlock;

        enif_alloc_binary(blockSize, &tmpBlock);

        // Alignment with respect to each other along 16-byte Boundary
        if (i == 0)
            align = (unsigned long)tmpBlock.data & 0x0f;
        else if (((unsigned long)tmpBlock.data & 0x0f) != align){
            aligned = false;
        }

        dataBlocks[i] = (char*)tmpBlock.data;

        allBlockEntry.push_back(tmpBlock);
        
        // Setup Data Blocks
        if (offset < dataSize) {
            size_t copySize = min(dataSize - offset, blockSize);
            if (copySize < blockSize)
                memset(dataBlocks[i], 0, blockSize);
            memcpy(dataBlocks[i], data + offset, copySize);
            offset += copySize;
        } else {
            memset(dataBlocks[i], 0, blockSize);        
        }
    }
    
    for(int i = 0; i < m; ++i) {
        ErlNifBinary tmpBlock;

        enif_alloc_binary(blockSize, &tmpBlock);
        if (((unsigned long)tmpBlock.data & 0x0f) != align){
            aligned = false;
        }
        codeBlocks[i] = (char*)tmpBlock.data;

        allBlockEntry.push_back(tmpBlock);
    }

    char* tmpMemory = NULL;
    // Encode in Pre-allocated Space Instead
    if (!aligned) {
        tmpMemory = (char*)alloc(blockSize * (k + m));
        memset(tmpMemory, 0, blockSize * (k + m));
        memcpy(tmpMemory, data, dataSize);
        for(int i = 0; i < k + m; ++i) {
            (i < k) ? dataBlocks[i] = tmpMemory + i * blockSize :
                codeBlocks[i - k] = tmpMemory + i * blockSize;
        }
    }

    jerasure_matrix_encode(k, m, w, matrix, dataBlocks, codeBlocks, blockSize);
    
    // Copy Back the Code Blocks
    if (!aligned) {
        for(int i = 0; i < m; ++i) {
            memcpy(allBlockEntry[i + k].data, codeBlocks[i], blockSize);
        }
        dealloc(tmpMemory);
    }
    free(matrix);
    dealloc(dataBlocks);
    dealloc(codeBlocks);
    return allBlockEntry;
}

ErlNifBinary RSCoding::doDecode(vector<ErlNifBinary> blockList, vector<int> blockIdList, size_t dataSize) {

    ErlNifBinary file;

    set<int> availSet(blockIdList.begin(), blockIdList.end());
    if (availSet.size() < (unsigned int)k) 
        throw std::invalid_argument("Not Enough Blocks");

    size_t blockSize = blockList[0].size;
    char** dataBlocks = (char**)alloc(sizeof(char*) * k);
    char** codeBlocks = (char**)alloc(sizeof(char*) * m);
    int erasures[k + m];
    char* tmpMemory = (char*)alloc(blockSize * (k + m));

    int j = 0;
    for(int i = 0; i < k + m; ++i) {
        i < k ? dataBlocks[i] = tmpMemory + i * blockSize : codeBlocks[i - k] = tmpMemory + i * blockSize;
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

    int *matrix = reed_sol_vandermonde_coding_matrix(k, m, w);
    jerasure_matrix_decode(k, m, w, matrix, 1, erasures, dataBlocks, codeBlocks, blockSize);

    enif_alloc_binary(dataSize, &file);
    memcpy(file.data, tmpMemory, dataSize);

    dealloc(tmpMemory);

    free(matrix);
    dealloc(dataBlocks);
    dealloc(codeBlocks);
    return file;
}
