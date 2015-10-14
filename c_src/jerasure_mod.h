// -------------------------------------------------------------------
//
// leo_erasure: Erasure code library for Erlang
//
// Copyright (c) 2012-2015 Rakuten, Inc.
//
// This file is provided to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file
// except in compliance with the License.  You may obtain
// a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.
//
// -------------------------------------------------------------------
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
