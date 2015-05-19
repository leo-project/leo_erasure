#include "coding.h"

vector<ERL_NIF_TERM> Coding::doRepair(vector<ERL_NIF_TERM> blockList, vector<int> blockIdList, vector<int> repairList) {
    ErlNifBinary bin;
    enif_inspect_binary(env, blockList[0], &bin);
    ERL_NIF_TERM file = doDecode(blockList, blockIdList, k * bin.size);
    vector<ERL_NIF_TERM> blocks = doEncode(file); 
    
    vector<ERL_NIF_TERM> repairBlocks;
    for (vector<int>::iterator it = repairList.begin(); it != repairList.end(); ++it) {
        repairBlocks.push_back(blocks[*it]);
    }
    return repairBlocks;
}
