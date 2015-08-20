#ifndef __I_RS_CODING_H__
#define __I_RS_CODING_H__

#include "coding.h"

class IRSCoding : public Coding {
    public:
        IRSCoding(int k, int m, int w, ErlNifEnv* env) : Coding(k, m, w, env) {};
        vector<ERL_NIF_TERM> doEncode(ERL_NIF_TERM dataBin);
        ERL_NIF_TERM doDecode(vector<ERL_NIF_TERM> blockList, vector<int> blockIdList, size_t dataSize);
        vector<ERL_NIF_TERM> doRepair(vector<ERL_NIF_TERM> blockList, vector<int> blockIdList, vector<int> repairList);
        void checkParams();
    private:
    void gf_gen_decode_matrix(unsigned char* encode_matrix, unsigned char* decode_matrix, vector<int> availBlockIdList,vector<int> outBlockIdList);
};

#endif
