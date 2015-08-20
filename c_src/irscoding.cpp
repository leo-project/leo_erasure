#include <string.h>
#include <set>

#include "irscoding.h"

#include "erasure_code.h"

#include "jerasure.h"
#include "jerasure_mod.h"
#include "reed_sol.h"

void IRSCoding::checkParams() {
    if (k <= 0 || m <= 0 || w <= 0)
        throw std::invalid_argument("Invalid Coding Parameters");
	if (w != 8 && w != 16 && w != 32) 
        throw std::invalid_argument("Invalid Coding Parameters (w = 8/16/32)");
}

vector<ERL_NIF_TERM> IRSCoding::doEncode(ERL_NIF_TERM dataBin) {
    int *matrix = reed_sol_vandermonde_coding_matrix(k, m, w);

    char* dataBlocks[k];
    char* codeBlocks[m];

    ErlNifBinary data;
    enif_inspect_binary(env, dataBin, &data);

    size_t dataSize = data.size;
    size_t blockSize = roundTo((roundTo(dataSize, k*w) / (k*w)), 16) * w;

    size_t offset = 0;
    size_t remain = dataSize;
    int filled = 0;
    while(remain >= blockSize) {
        dataBlocks[filled] = (char*)data.data + offset;
        offset += blockSize;
        remain -= blockSize;
        filled++;
    }
    ErlNifBinary tmp;
    enif_alloc_binary((k + m - filled) * blockSize + 16, &tmp);
    size_t align = (((size_t)data.data & 0x0f) - ((size_t)tmp.data & 0x0f) + 16) & 0x0f;
    char* alignedHead = (char*)tmp.data + align;
    memcpy(alignedHead, data.data + filled * blockSize, dataSize - filled * blockSize);
    offset = 0;
    for(int i = filled; i < k + m; ++i, offset += blockSize) {
        (i < k) ? dataBlocks[i] = alignedHead + offset:
            codeBlocks[i - k] = alignedHead + offset;
    }

    uint8_t a[32 * 32];
    uint8_t g_tbls[32 * 32 * 32];
    gf_gen_rs_matrix(a, k + m, k);
    ec_init_tables(k, m, &a[k * k], g_tbls);
    ec_encode_data(blockSize, k, m, g_tbls, (unsigned char**)dataBlocks, (unsigned char**)codeBlocks);
//    jerasure_matrix_encode(k, m, w, matrix, dataBlocks, codeBlocks, blockSize);

    vector<ERL_NIF_TERM> blockList;
    for(int i = 0; i < filled; ++i) {
        blockList.push_back(enif_make_sub_binary(env, dataBin, i * blockSize, blockSize)); 
    }
    ERL_NIF_TERM tmpBin = enif_make_binary(env, &tmp);
    offset = 0;
    for(int i = filled; i < k + m; ++i, offset += blockSize) {
        blockList.push_back(enif_make_sub_binary(env, tmpBin, offset + align, blockSize));
    }

    free(matrix);

    return blockList;
}

static int gf_gen_decode_matrix(unsigned char *encode_matrix,
				unsigned char *decode_matrix,
				unsigned char *invert_matrix,
				unsigned int *decode_index,
				unsigned char *src_err_list,
				unsigned char *src_in_err,
				int nerrs, int nsrcerrs, int k, int m);

ERL_NIF_TERM IRSCoding::doDecode(vector<ERL_NIF_TERM> blockList, vector<int> blockIdList, size_t dataSize) {

    set<int> availSet(blockIdList.begin(), blockIdList.end());
    if (availSet.size() < (unsigned int)k) 
        throw std::invalid_argument("Not Enough Blocks");
    else if (availSet.size() < blockIdList.size()) {
        throw std::invalid_argument("Blocks should be unique");
    }

    size_t blockSize;

    ErlNifBinary blocks[k + m];
    for(size_t i = 0; i < blockIdList.size(); ++i) {
        int blockId = blockIdList[i];
        enif_inspect_binary(env, blockList[i], &blocks[blockId]);
        blockSize = blocks[blockId].size;
    }

    bool needFix = false;
    int dataErasures = 0;

    for(int i = 0; i < k; ++i) 
        if (availSet.count(i) == 0) {
            needFix = true;
            ++dataErasures;
        }

    if (!needFix) {
        ErlNifBinary file;
        enif_alloc_binary(dataSize, &file);
        size_t copySize, offset = 0;
        for(int i = 0; i < k; ++i) {
            offset = i * blockSize;
            copySize = min(dataSize - offset, blockSize);
            memcpy(file.data + offset, blocks[i].data, copySize);
        }
        ERL_NIF_TERM bin = enif_make_binary(env, &file);
        return bin;
    }

    char* dataBlocks[k];
    char* codeBlocks[m];
    unsigned char erasures[k + m];
    unsigned char failed[k + m];
    ErlNifBinary tmpBin;
    enif_alloc_binary(blockSize * (k + m), &tmpBin);
    char* tmpMemory = (char*)tmpBin.data;

    unsigned char* out[32];

    int j = 0;
    for(int i = 0; i < k + m; ++i) {
        i < k ? dataBlocks[i] = tmpMemory + i * blockSize : codeBlocks[i - k] = tmpMemory + i * blockSize;
        if (availSet.count(i) == 0) {
            out[j] = (unsigned char*)tmpMemory + i * blockSize;
            erasures[j++] = i;
            failed[i] = 1;
        } else {
            failed[i] = 0;
            memcpy(tmpMemory + i * blockSize, blocks[i].data, blockSize);
        }
    }
    erasures[j] = -1;

    uint8_t a[32 * 32];
    uint8_t decode_matrix[32 * 32];
    uint8_t invert_matrix[32 * 32];
    uint8_t g_tbls[32 * 32 * 32];
    unsigned int decode_index[32];
    gf_gen_rs_matrix(a, k + m, k);
    ec_init_tables(k, m, &a[k * k], g_tbls);

    unsigned char* recov[32];

    gf_gen_decode_matrix(a, decode_matrix, invert_matrix, decode_index, erasures, failed, j, dataErasures, k, k + m);

    for (int i = 0; i < k; ++i) {
        decode_index[i] < k ? recov[i] = (unsigned char*)dataBlocks[decode_index[i]] :
            recov[i] = (unsigned char*)codeBlocks[decode_index[i] - k];
    }
    ec_init_tables(k, j, decode_matrix, g_tbls);
    ec_encode_data(blockSize, k, j, g_tbls, recov, out);
//    int *matrix = reed_sol_vandermonde_coding_matrix(k, m, w);
//    jerasure_matrix_decode_data(k, m, w, matrix, 1, erasures, dataBlocks, codeBlocks, blockSize);

    ERL_NIF_TERM allBlocksBin = enif_make_binary(env, &tmpBin);
    ERL_NIF_TERM bin = enif_make_sub_binary(env, allBlocksBin, 0, dataSize);

//    free(matrix);
    return bin;
}

vector<ERL_NIF_TERM> IRSCoding::doRepair(vector<ERL_NIF_TERM> blockList, vector<int> blockIdList, vector<int> repairList) {

    set<int> availSet(blockIdList.begin(), blockIdList.end());
    if (availSet.size() < (unsigned int)k) 
        throw std::invalid_argument("Not Enough Blocks");
    else if (availSet.size() < blockIdList.size()) {
        throw std::invalid_argument("Blocks should be unique");
    }

    size_t blockSize;

    ErlNifBinary blocks[k + m];
    for(size_t i = 0; i < blockIdList.size(); ++i) {
        int blockId = blockIdList[i];
        enif_inspect_binary(env, blockList[i], &blocks[blockId]);
        blockSize = blocks[blockId].size;
    }

    char* dataBlocks[k];
    char* codeBlocks[m];
    int erasures[k + m];
    ErlNifBinary tmpBin;
    enif_alloc_binary(blockSize * (k + m), &tmpBin);
    char* tmpMemory = (char*)tmpBin.data;

    int j = 0;
    for(int i = 0; i < k + m; ++i) {
        i < k ? dataBlocks[i] = tmpMemory + i * blockSize : codeBlocks[i - k] = tmpMemory + i * blockSize;
        if (availSet.count(i) == 0) {
            erasures[j++] = i;
        } else {
            memcpy(tmpMemory + i * blockSize, blocks[i].data, blockSize);
        }
    }
    erasures[j] = -1;

    repairList.push_back(-1);
    int *selected = &repairList[0];
    int *matrix = reed_sol_vandermonde_coding_matrix(k, m, w);
    jerasure_matrix_decode_selected(k, m, w, matrix, 1, erasures, selected, dataBlocks, codeBlocks, blockSize);
    
    vector<ERL_NIF_TERM> repairBlocks;
    int repairId;
    for(size_t i = 0; i < repairList.size() - 1; ++i) {
        repairId = repairList[i];
        ERL_NIF_TERM allBlocksBin = enif_make_binary(env, &tmpBin);
        ERL_NIF_TERM block = enif_make_sub_binary(env, allBlocksBin, repairId * blockSize, blockSize); 
        repairBlocks.push_back(block);
    }

    free(matrix);
    return repairBlocks; 
}

#define NO_INVERT_MATRIX -2
// Generate decode matrix from encode matrix
static int gf_gen_decode_matrix(unsigned char *encode_matrix,
				unsigned char *decode_matrix,
				unsigned char *invert_matrix,
				unsigned int *decode_index,
				unsigned char *src_err_list,
				unsigned char *src_in_err,
				int nerrs, int nsrcerrs, int k, int m)
{
	int i, j, p;
	int r;
	unsigned char *backup, *b, s;
	int incr = 0;

	b = (unsigned char*)malloc(32 * 32);
	backup = (unsigned char*)malloc(32 * 32);

	if (b == NULL || backup == NULL) {
		printf("Test failure! Error with malloc\n");
		free(b);
		free(backup);
		return -1;
	}
	// Construct matrix b by removing error rows
	for (i = 0, r = 0; i < k; i++, r++) {
		while (src_in_err[r])
			r++;
		for (j = 0; j < k; j++) {
			b[k * i + j] = encode_matrix[k * r + j];
			backup[k * i + j] = encode_matrix[k * r + j];
		}
		decode_index[i] = r;
	}
	incr = 0;
	while (gf_invert_matrix(b, invert_matrix, k) < 0) {
		if (nerrs == (m - k)) {
			free(b);
			free(backup);
			printf("BAD MATRIX\n");
			return NO_INVERT_MATRIX;
		}
		incr++;
		memcpy(b, backup, 32 * 32);
		for (i = nsrcerrs; i < nerrs - nsrcerrs; i++) {
			if (src_err_list[i] == (decode_index[k - 1] + incr)) {
				// skip the erased parity line
				incr++;
				continue;
			}
		}
		if (decode_index[k - 1] + incr >= m) {
			free(b);
			free(backup);
			printf("BAD MATRIX\n");
			return NO_INVERT_MATRIX;
		}
		decode_index[k - 1] += incr;
		for (j = 0; j < k; j++)
			b[k * (k - 1) + j] = encode_matrix[k * decode_index[k - 1] + j];

	};

	for (i = 0; i < nsrcerrs; i++) {
		for (j = 0; j < k; j++) {
			decode_matrix[k * i + j] = invert_matrix[k * src_err_list[i] + j];
		}
	}
	/* src_err_list from encode_matrix * invert of b for parity decoding */
	for (p = nsrcerrs; p < nerrs; p++) {
		for (i = 0; i < k; i++) {
			s = 0;
			for (j = 0; j < k; j++)
				s ^= gf_mul(invert_matrix[j * k + i],
					    encode_matrix[k * src_err_list[p] + j]);

			decode_matrix[k * p + i] = s;
		}
	}
	free(b);
	free(backup);
	return 0;
}
