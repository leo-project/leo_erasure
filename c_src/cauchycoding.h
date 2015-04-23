#ifndef __Cauchy_CODING_H__
#define __Cauchy_CODING_H__

#include "coding.h"

class CauchyCoding : public Coding {
    public:
        CauchyCoding(int k, int m, int w) : Coding(k, m, w) {};
        vector<ErlNifBinary> doEncode(unsigned char* data, size_t dataSize);
        ErlNifBinary doDecode(vector<ErlNifBinary> blockList, vector<int> blockIdList, size_t dataSize);

//        vector<BlockEntry> doEncode(unsigned char* data, size_t dataSize);
//        BlockEntry doDecode(vector<BlockEntry> blockList, vector<int> blockIdList, size_t dataSize);
};

#endif
