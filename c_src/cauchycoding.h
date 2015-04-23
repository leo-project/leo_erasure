#ifndef __CAUCHY_CODING_H__
#define __CAUCHY_CODING_H__

#include "coding.h"

class CAUCHYCoding : public Coding {
    public:
        CAUCHYCoding(int k, int m, int w) : Coding(k, m, w) {};
        vector<ErlNifBinary> doEncode(unsigned char* data, size_t dataSize);
        ErlNifBinary doDecode(vector<ErlNifBinary> blockList, vector<int> blockIdList, size_t dataSize);

//        vector<BlockEntry> doEncode(unsigned char* data, size_t dataSize);
//        BlockEntry doDecode(vector<BlockEntry> blockList, vector<int> blockIdList, size_t dataSize);
};

#endif
