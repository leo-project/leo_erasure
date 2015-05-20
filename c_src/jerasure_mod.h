#pragma once

#ifndef _JERASURE_MOD_H
#define _JERASURE_MOD_H
#include "jerasure.h"
int jerasure_matrix_decode_data(int k, int m, int w, 
        int *matrix, int row_k_ones, int *erasures,
        char **data_ptrs, char **coding_ptrs, int size);
int jerasure_matrix_decode_selected(int k, int m, int w, 
        int *matrix, int row_k_ones, int *erasures,
        int *selected, 
        char **data_ptrs, char **coding_ptrs, int size);
int jerasure_schedule_decode_data_lazy(int k, int m, int w, 
        int *bitmatrix, int *erasures, 
        char **data_ptrs, char **coding_ptrs, int size, int packetsize,
        int smart);
int jerasure_schedule_decode_selected_lazy(int k, int m, int w, 
        int *bitmatrix, int *erasures, 
        int *selected,
        char **data_ptrs, char **coding_ptrs, int size, int packetsize,
        int smart);
#endif
