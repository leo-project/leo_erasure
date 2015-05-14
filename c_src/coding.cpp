#include "coding.h"

ErlNifBinary Coding::doRepair(vector<ErlNifBinary> blockList, vector<int> blockIdList, int repairId) {
    ErlNifBinary dec = doDecode(blockList, blockIdList, k * blockList[0].size);
    vector<ErlNifBinary> enc = doEncode(dec.data, k * blockList[0].size);
    enif_release_binary(&dec);
    for(size_t i = 0; i < enc.size(); ++i) {
        if (i != (size_t)repairId)
            enif_release_binary(&dec);
    }
    return enc[repairId];
}
