#pragma once

#ifndef _JERASURE_MOD_H
#define _JERASURE_MOD_H
#include "jerasure.h"
int jerasure_matrix_decode_data(int k, int m, int w, 
        int *matrix, int row_k_ones, int *erasures,
        char **data_ptrs, char **coding_ptrs, int size);
int jerasure_schedule_decode_data_lazy(int k, int m, int w, 
        int *bitmatrix, int *erasures, 
        char **data_ptrs, char **coding_ptrs, int size, int packetsize,
        int smart);
int **jerasure_generate_decoding_data_schedule(
        int k, int m, int w, int *bitmatrix, int *erasures, int smart);

// Unmodified Private Functions
char **set_up_ptrs_for_scheduled_decoding(
        int k, int m, int *erasures, char **data_ptrs, char **coding_ptrs);
int set_up_ids_for_scheduled_decoding(
        int k, int m, int *erasures, int *row_ids, int *ind_to_row);
#endif
