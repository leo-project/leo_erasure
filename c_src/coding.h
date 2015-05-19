#ifndef __CODING_H__
#define __CODING_H__

#include <vector>
#include <stdexcept>
using namespace std;

#include "common.h"

#include "erl_nif.h"

class Coding {
    public:
        Coding(int _k, int _m, int _w, ErlNifEnv* _env) : k(_k), m(_m), w(_w), env(_env) {};

        virtual vector<ERL_NIF_TERM> doEncode(ERL_NIF_TERM dataBin) = 0;
        virtual ERL_NIF_TERM doDecode(vector<ERL_NIF_TERM> blockList, vector<int> blockIdList, size_t dataSize) = 0;
        virtual vector<ERL_NIF_TERM> doRepair(vector<ERL_NIF_TERM> blockList, vector<int> blockIdList, vector<int> repairList);
        virtual ERL_NIF_TERM doRepair(vector<ERL_NIF_TERM> blockList, vector<int> blockIdList, int repairId);
        virtual void checkParams() = 0;
    protected:
        int k, m, w;
        ErlNifEnv* env;
};

#endif
