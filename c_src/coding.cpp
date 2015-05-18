#include "coding.h"

ERL_NIF_TERM Coding::doEncode(ErlNifEnv* env, ERL_NIF_TERM dataBin) {
    ErlNifBinary data;
    enif_inspect_binary(env, dataBin, &data);
    vector<ErlNifBinary> blocks = doEncode(data.data, data.size);

    ERL_NIF_TERM retArr[blocks.size()];
    for(unsigned int i = 0; i < blocks.size(); ++i) {
        retArr[i] = enif_make_binary(env, &blocks[i]);
    }
    ERL_NIF_TERM blockList = enif_make_list_from_array(env, retArr, blocks.size());
    return blockList;
}

ErlNifBinary Coding::doRepair(vector<ErlNifBinary> blockList, vector<int> blockIdList, int repairId) {
    ErlNifBinary dec = doDecode(blockList, blockIdList, k * blockList[0].size);
    vector<ErlNifBinary> enc = doEncode(dec.data, k * blockList[0].size);
    enif_release_binary(&dec);
    for(size_t i = 0; i < enc.size(); ++i) {
        if (i != (size_t)repairId)
            enif_release_binary(&enc[i]);
    }
    return enc[repairId];
}
