#ifndef __LIBERATION_CODING_H__
#define __LIBERATION_CODING_H__

#include "coding.h"

class LiberationCoding : public Coding {
    public:
        LiberationCoding(int k, int m, int w) : Coding(k, m, w) {};
        vector<ErlNifBinary> doEncode(unsigned char* data, size_t dataSize);
        ErlNifBinary doDecode(vector<ErlNifBinary> blockList, vector<int> blockIdList, size_t dataSize);
        void checkParams();
};

#endif
