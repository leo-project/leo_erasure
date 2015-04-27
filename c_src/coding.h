#ifndef __CODING_H__
#define __CODING_H__

#include <vector>
#include <stdexcept>
using namespace std;

#include "common.h"

#include "erl_nif.h"

class Coding {
    public:
        Coding(int _k, int _m, int _w) : k(_k), m(_m), w(_w) {};

        virtual vector<ErlNifBinary> doEncode(unsigned char* data, size_t dataSize) = 0;
        virtual ErlNifBinary doDecode(vector<ErlNifBinary> blockList, vector<int> blockIdList, size_t dataSize) = 0;
        virtual void checkParams() = 0;
    protected:
        int k, m, w;
};

#endif
