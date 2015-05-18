#include "coding.h"

ERL_NIF_TERM Coding::doRepair(vector<ERL_NIF_TERM> blockList, vector<int> blockIdList, int repairId) {
    ErlNifBinary bin;
    enif_inspect_binary(env, blockList[0], &bin);
    ERL_NIF_TERM file = doDecode(blockList, blockIdList, k * bin.size);
    vector<ERL_NIF_TERM> blocks = doEncode(file); 
    return blocks[repairId];
}
