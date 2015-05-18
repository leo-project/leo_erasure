#ifndef __RS_CODING_H__
#define __RS_CODING_H__

#include "coding.h"

class RSCoding : public Coding {
    public:
        RSCoding(int k, int m, int w) : Coding(k, m, w) {};
        vector<ErlNifBinary> doEncode(unsigned char* data, size_t dataSize);
        ERL_NIF_TERM doEncode(ErlNifEnv* env, ERL_NIF_TERM dataBin);
        ErlNifBinary doDecode(vector<ErlNifBinary> blockList, vector<int> blockIdList, size_t dataSize);
        void checkParams();
};

#endif
