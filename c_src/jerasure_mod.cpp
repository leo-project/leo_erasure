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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "jerasure_mod.h"

#define talloc(type, num) (type *) malloc(sizeof(type)*(num))

// Modified Private Functions
static int **jerasure_generate_decoding_data_schedule(
        int k, int m, int w, int *bitmatrix, int *erasures, int smart);

// Unmodified Private Functions
static char **set_up_ptrs_for_scheduled_decoding(
        int k, int m, int *erasures, char **data_ptrs, char **coding_ptrs);
static int set_up_ids_for_scheduled_decoding(
        int k, int m, int *erasures, int *row_ids, int *ind_to_row);

// Extra Private Functions
static int **jerasure_generate_decoding_selected_schedule(
        int k, int m, int w, int *bitmatrix,
        int *erasures, int *selected, int smart);

void convert_select(int k, int m, int* selected, int *data_fix, int *data_fix_size, int *code_fix, int *code_fix_size) {
    *data_fix_size = 0;
    *code_fix_size = 0;
    for(int i = 0; i < k + m; ++i) {
        if (selected[i] == -1) break;
        else if (selected[i] < k) {
            data_fix[*data_fix_size] = selected[i];
            *data_fix_size += 1;
        } else {
            code_fix[*code_fix_size] = selected[i];
            *code_fix_size += 1;
        }
    }
}

/*********************
 *                   *
 *       APIs        *
 *                   *
 *********************/
int jerasure_matrix_decode_data(int k, int m, int w, int *matrix, int row_k_ones, int *erasures,
        char **data_ptrs, char **coding_ptrs, int size)
{
    int i, edd, lastdrive;
    int *tmpids;
    int *erased, *decoding_matrix, *dm_ids;

    if (w != 8 && w != 16 && w != 32) return -1;

    erased = jerasure_erasures_to_erased(k, m, erasures);
    if (erased == NULL) return -1;

    /* Find the number of data drives failed */

    lastdrive = k;

    edd = 0;
    for (i = 0; i < k; i++) {
        if (erased[i]) {
            edd++;
            lastdrive = i;
        }
    }

    /* You only need to create the decoding matrix in the following cases:

       1. edd > 0 and row_k_ones is false.
       2. edd > 0 and row_k_ones is true and coding device 0 has been erased.
       3. edd > 1

       We're going to use lastdrive to denote when to stop decoding data.
       At this point in the code, it is equal to the last erased data device.
       However, if we can't use the parity row to decode it (i.e. row_k_ones=0
       or erased[k] = 1, we're going to set it to k so that the decoding
       pass will decode all data.
       */

    if (!row_k_ones || erased[k]) lastdrive = k;

    dm_ids = NULL;
    decoding_matrix = NULL;

    if (edd > 1 || (edd > 0 && (!row_k_ones || erased[k]))) {
        dm_ids = talloc(int, k);
        if (dm_ids == NULL) {
            free(erased);
            return -1;
        }

        decoding_matrix = talloc(int, k*k);
        if (decoding_matrix == NULL) {
            free(erased);
            free(dm_ids);
            return -1;
        }

        if (jerasure_make_decoding_matrix(k, m, w, matrix, erased, decoding_matrix, dm_ids) < 0) {
            free(erased);
            free(dm_ids);
            free(decoding_matrix);
            return -1;
        }
    }

    /* Decode the data drives.
       If row_k_ones is true and coding device 0 is intact, then only decode edd-1 drives.
       This is done by stopping at lastdrive.
       We test whether edd > 0 so that we can exit the loop early if we're done.
       */

    for (i = 0; edd > 0 && i < lastdrive; i++) {
        if (erased[i]) {
            jerasure_matrix_dotprod(k, w, decoding_matrix+(i*k), dm_ids, i, data_ptrs, coding_ptrs, size);
            edd--;
        }
    }

    /* Then if necessary, decode drive lastdrive */

    if (edd > 0) {
        tmpids = talloc(int, k);
        if (!tmpids) {
            free(erased);
            free(dm_ids);
            free(decoding_matrix);
            return -1;
        }
        for (i = 0; i < k; i++) {
            tmpids[i] = (i < lastdrive) ? i : i+1;
        }
        jerasure_matrix_dotprod(k, w, matrix, tmpids, lastdrive, data_ptrs, coding_ptrs, size);
        free(tmpids);
    }

    free(erased);
    if (dm_ids != NULL) free(dm_ids);
    if (decoding_matrix != NULL) free(decoding_matrix);

    return 0;
}

int jerasure_matrix_decode_selected(int k, int m, int w, int *matrix, int row_k_ones, int *erasures, int *selected,
        char **data_ptrs, char **coding_ptrs, int size)
{
    int i, edd, lastdrive;
    int *tmpids;
    int *erased, *decoding_matrix, *dm_ids;

    if (w != 8 && w != 16 && w != 32) return -1;

    erased = jerasure_erasures_to_erased(k, m, erasures);
    if (erased == NULL) return -1;

    /* Find the number of data drives failed */

    lastdrive = k;

    edd = 0;
    for (i = 0; i < k; i++) {
        if (erased[i]) {
            edd++;
            lastdrive = i;
        }
    }

    /* You only need to create the decoding matrix in the following cases:

       1. edd > 0 and row_k_ones is false.
       2. edd > 0 and row_k_ones is true and coding device 0 has been erased.
       3. edd > 1

       We're going to use lastdrive to denote when to stop decoding data.
       At this point in the code, it is equal to the last erased data device.
       However, if we can't use the parity row to decode it (i.e. row_k_ones=0
       or erased[k] = 1, we're going to set it to k so that the decoding
       pass will decode all data.
       */

    if (!row_k_ones || erased[k]) lastdrive = k;

    dm_ids = NULL;
    decoding_matrix = NULL;

    if (edd > 1 || (edd > 0 && (!row_k_ones || erased[k]))) {
        dm_ids = talloc(int, k);
        if (dm_ids == NULL) {
            free(erased);
            return -1;
        }

        decoding_matrix = talloc(int, k*k);
        if (decoding_matrix == NULL) {
            free(erased);
            free(dm_ids);
            return -1;
        }

        if (jerasure_make_decoding_matrix(k, m, w, matrix, erased, decoding_matrix, dm_ids) < 0) {
            free(erased);
            free(dm_ids);
            free(decoding_matrix);
            return -1;
        }
    }

    /* Setup to indicate which blocks have to be repaired

       If any code block need to be fixed,
       all data missing blocks have to be repaired
       */
    int data_fix_size, code_fix_size = 0;
    int data_fix[k];
    int code_fix[m];
    convert_select(k, m, selected, data_fix, &data_fix_size, code_fix, &code_fix_size);
    bool fix_all_data = false;
    int target;
    if ((data_fix_size == edd) || (code_fix_size > 0))
        fix_all_data = true;

    if (fix_all_data) {
        /* Decode the data drives.
           If row_k_ones is true and coding device 0 is intact, then only decode edd-1 drives.
           This is done by stopping at lastdrive.
           We test whether edd > 0 so that we can exit the loop early if we're done.
           */

        for (i = 0; edd > 0 && i < lastdrive; i++) {
            if (erased[i]) {
                jerasure_matrix_dotprod(k, w, decoding_matrix+(i*k), dm_ids, i, data_ptrs, coding_ptrs, size);
                edd--;
            }
        }

        /* Then if necessary, decode drive lastdrive */
        if (edd > 0) {
            tmpids = talloc(int, k);
            if (!tmpids) {
                free(erased);
                free(dm_ids);
                free(decoding_matrix);
                return -1;
            }
            for (i = 0; i < k; i++) {
                tmpids[i] = (i < lastdrive) ? i : i+1;
            }
            jerasure_matrix_dotprod(k, w, matrix, tmpids, lastdrive, data_ptrs, coding_ptrs, size);
            free(tmpids);
        }
    } else {
        for (i = 0; edd > 0 && i < lastdrive; i++) {
            target = data_fix[i];
            jerasure_matrix_dotprod(k, w, decoding_matrix+(target*k), dm_ids, target, data_ptrs, coding_ptrs, size);
            edd--;
        }
    }

    /* Finally, re-encode coding devices */
    for (i = 0; i < code_fix_size; i++) {
        target = code_fix[i] - k;
        jerasure_matrix_dotprod(k, w, matrix+(target*k), NULL, target+k, data_ptrs, coding_ptrs, size);
    }

    free(erased);
    if (dm_ids != NULL) free(dm_ids);
    if (decoding_matrix != NULL) free(decoding_matrix);

    return 0;
}

int jerasure_schedule_decode_data_lazy(int k, int m, int w, int *bitmatrix, int *erasures,
        char **data_ptrs, char **coding_ptrs, int size, int packetsize,
        int smart)
{
    int i, tdone;
    char **ptrs;
    int **schedule;

    ptrs = set_up_ptrs_for_scheduled_decoding(k, m, erasures, data_ptrs, coding_ptrs);
    if (ptrs == NULL) return -1;

    schedule = jerasure_generate_decoding_data_schedule(k, m, w, bitmatrix, erasures, smart);
    if (schedule == NULL) {
        free(ptrs);
        return -1;
    }

    for (tdone = 0; tdone < size; tdone += packetsize*w) {
        jerasure_do_scheduled_operations(ptrs, schedule, packetsize);
        for (i = 0; i < k+m; i++) ptrs[i] += (packetsize*w);
    }

    jerasure_free_schedule(schedule);
    free(ptrs);

    return 0;
}

int jerasure_schedule_decode_selected_lazy(int k, int m, int w, int *bitmatrix, int *erasures,
        int * selected,
        char **data_ptrs, char **coding_ptrs, int size, int packetsize,
        int smart)
{
    int i, tdone;
    char **ptrs;
    int **schedule;

    ptrs = set_up_ptrs_for_scheduled_decoding(k, m, erasures, data_ptrs, coding_ptrs);
    if (ptrs == NULL) return -1;

    schedule = jerasure_generate_decoding_selected_schedule(k, m, w, bitmatrix, erasures, selected, smart);
    if (schedule == NULL) {
        free(ptrs);
        return -1;
    }

    for (tdone = 0; tdone < size; tdone += packetsize*w) {
        jerasure_do_scheduled_operations(ptrs, schedule, packetsize);
        for (i = 0; i < k+m; i++) ptrs[i] += (packetsize*w);
    }

    jerasure_free_schedule(schedule);
    free(ptrs);

    return 0;
}


/*********************
 *                   *
 * Private Functions *
 *                   *
 *********************/
static char **set_up_ptrs_for_scheduled_decoding(int k, int m, int *erasures, char **data_ptrs, char **coding_ptrs)
{
    int ddf, cdf;
    int *erased;
    char **ptrs;
    int i, j, x;

    ddf = 0;
    cdf = 0;
    for (i = 0; erasures[i] != -1; i++) {
        if (erasures[i] < k) ddf++; else cdf++;
    }

    erased = jerasure_erasures_to_erased(k, m, erasures);
    if (erased == NULL) return NULL;

    /* Set up ptrs.  It will be as follows:

       - If data drive i has not failed, then ptrs[i] = data_ptrs[i].
       - If data drive i has failed, then ptrs[i] = coding_ptrs[j], where j is the
       lowest unused non-failed coding drive.
       - Elements k to k+ddf-1 are data_ptrs[] of the failed data drives.
       - Elements k+ddf to k+ddf+cdf-1 are coding_ptrs[] of the failed data drives.

       The array row_ids contains the ids of ptrs.
       The array ind_to_row_ids contains the row_id of drive i.

       However, we're going to set row_ids and ind_to_row in a different procedure.
       */

    ptrs = talloc(char *, k+m);
    if (!ptrs) {
        free(erased);
        return NULL;
    }

    j = k;
    x = k;
    for (i = 0; i < k; i++) {
        if (erased[i] == 0) {
            ptrs[i] = data_ptrs[i];
        } else {
            while (erased[j]) j++;
            ptrs[i] = coding_ptrs[j-k];
            j++;
            ptrs[x] = data_ptrs[i];
            x++;
        }
    }
    for (i = k; i < k+m; i++) {
        if (erased[i]) {
            ptrs[x] = coding_ptrs[i-k];
            x++;
        }
    }
    free(erased);
    return ptrs;
}

static int set_up_ids_for_scheduled_decoding(int k, int m, int *erasures, int *row_ids, int *ind_to_row)
{
    int ddf, cdf;
    int *erased;
    int i, j, x;

    ddf = 0;
    cdf = 0;
    for (i = 0; erasures[i] != -1; i++) {
        if (erasures[i] < k) ddf++; else cdf++;
    }

    erased = jerasure_erasures_to_erased(k, m, erasures);
    if (erased == NULL) return -1;

    /* See set_up_ptrs_for_scheduled_decoding for how these are set */

    j = k;
    x = k;
    for (i = 0; i < k; i++) {
        if (erased[i] == 0) {
            row_ids[i] = i;
            ind_to_row[i] = i;
        } else {
            while (erased[j]) j++;
            row_ids[i] = j;
            ind_to_row[j] = i;
            j++;
            row_ids[x] = i;
            ind_to_row[i] = x;
            x++;
        }
    }
    for (i = k; i < k+m; i++) {
        if (erased[i]) {
            row_ids[x] = i;
            ind_to_row[i] = x;
            x++;
        }
    }
    free(erased);
    return 0;
}

static int **jerasure_generate_decoding_data_schedule(int k, int m, int w, int *bitmatrix, int *erasures, int smart)
{
    //int i, j, x, drive, y, index, z;
    int i,x;
    int *decoding_matrix, *inverse, *real_decoding_matrix;
    int *ptr;
    int *row_ids;
    int *ind_to_row;
    int ddf, cdf;
    int **schedule;
    //int *b1, *b2;

    /* First, figure out the number of data drives that have failed, and the
       number of coding drives that have failed: ddf and cdf */

    ddf = 0;
    cdf = 0;
    for (i = 0; erasures[i] != -1; i++) {
        if (erasures[i] < k) ddf++; else cdf++;
    }

    row_ids = talloc(int, k+m);
    if (!row_ids) return NULL;
    ind_to_row = talloc(int, k+m);
    if (!ind_to_row) {
        free(row_ids);
        return NULL;
    }

    if (set_up_ids_for_scheduled_decoding(k, m, erasures, row_ids, ind_to_row) < 0) {
        free(row_ids);
        free(ind_to_row);
        return NULL;
    }

    /* Now, we're going to create one decoding matrix which is going to
       decode everything with one call.  The hope is that the scheduler
       will do a good job.    This matrix has w*e rows, where e is the
       number of erasures (ddf+cdf) */

    real_decoding_matrix = talloc(int, k*w*(cdf+ddf)*w);
    if (!real_decoding_matrix) {
        free(row_ids);
        free(ind_to_row);
        return NULL;
    }

    /* First, if any data drives have failed, then initialize the first
       ddf*w rows of the decoding matrix from the standard decoding
       matrix inversion */

    if (ddf > 0) {

        decoding_matrix = talloc(int, k*k*w*w);
        if (!decoding_matrix) {
            free(row_ids);
            free(ind_to_row);
            return NULL;
        }
        ptr = decoding_matrix;
        for (i = 0; i < k; i++) {
            if (row_ids[i] == i) {
                bzero(ptr, k*w*w*sizeof(int));
                for (x = 0; x < w; x++) {
                    ptr[x+i*w+x*k*w] = 1;
                }
            } else {
                memcpy(ptr, bitmatrix+k*w*w*(row_ids[i]-k), k*w*w*sizeof(int));
            }
            ptr += (k*w*w);
        }
        inverse = talloc(int, k*k*w*w);
        if (!inverse) {
            free(row_ids);
            free(ind_to_row);
            free(decoding_matrix);
            return NULL;
        }
        jerasure_invert_bitmatrix(decoding_matrix, inverse, k*w);

        /*    printf("\nMatrix to invert\n");
              jerasure_print_bitmatrix(decoding_matrix, k*w, k*w, w);
              printf("\n");
              printf("\nInverse\n");
              jerasure_print_bitmatrix(inverse, k*w, k*w, w);
              printf("\n"); */

        free(decoding_matrix);
        ptr = real_decoding_matrix;
        for (i = 0; i < ddf; i++) {
            memcpy(ptr, inverse+k*w*w*row_ids[k+i], sizeof(int)*k*w*w);
            ptr += (k*w*w);
        }
        free(inverse);
    }

    /*
       printf("\n\nReal Decoding Matrix\n\n");
       jerasure_print_bitmatrix(real_decoding_matrix, (ddf+cdf)*w, k*w, w);
       printf("\n"); */
    if (smart) {
        schedule = jerasure_smart_bitmatrix_to_schedule(k, ddf, w, real_decoding_matrix);
    } else {
        schedule = jerasure_dumb_bitmatrix_to_schedule(k, ddf, w, real_decoding_matrix);
    }
    free(row_ids);
    free(ind_to_row);
    free(real_decoding_matrix);
    return schedule;
}

static int **jerasure_generate_decoding_selected_schedule(
        int k, int m, int w, int *bitmatrix,
        int *erasures, int* selected, int smart)
{
    int i, j, x, drive, y, index, z;
    int *decoding_matrix, *inverse, *real_decoding_matrix;
    int *ptr;
    int *row_ids;
    int *ind_to_row;
    int ddf, cdf;
    int **schedule;
    int *b1, *b2;

    /* First, figure out the number of data drives that have failed, and the
       number of coding drives that have failed: ddf and cdf */

    ddf = 0;
    cdf = 0;
    for (i = 0; erasures[i] != -1; i++) {
        if (erasures[i] < k) ddf++; else cdf++;
    }

    row_ids = talloc(int, k+m);
    if (!row_ids) return NULL;
    ind_to_row = talloc(int, k+m);
    if (!ind_to_row) {
        free(row_ids);
        return NULL;
    }

    if (set_up_ids_for_scheduled_decoding(k, m, erasures, row_ids, ind_to_row) < 0) {
        free(row_ids);
        free(ind_to_row);
        return NULL;
    }

    int data_fix_size, code_fix_size = 0;
    int data_fix[k];
    int code_fix[m];
    convert_select(k, m, selected, data_fix, &data_fix_size, code_fix, &code_fix_size);
    bool fix_all_data = false;
    if ((data_fix_size == ddf) || (code_fix_size > 0))
        fix_all_data = true;
    /* Now, we're going to create one decoding matrix which is going to
       decode everything with one call.  The hope is that the scheduler
       will do a good job.    This matrix has w*e rows, where e is the
       number of erasures (ddf+cdf) */

    int decoding_matrix_size = data_fix_size;
    if (fix_all_data)
        decoding_matrix_size = ddf + code_fix_size;

    real_decoding_matrix = talloc(int, k*w*w*decoding_matrix_size);
    if (!real_decoding_matrix) {
        free(row_ids);
        free(ind_to_row);
        return NULL;
    }

    /* First, if any data drives have failed, then initialize the first
       ddf*w rows of the decoding matrix from the standard decoding
       matrix inversion */

    if (ddf > 0) {

        decoding_matrix = talloc(int, k*k*w*w);
        if (!decoding_matrix) {
            free(row_ids);
            free(ind_to_row);
            return NULL;
        }
        ptr = decoding_matrix;
        for (i = 0; i < k; i++) {
            if (row_ids[i] == i) {
                bzero(ptr, k*w*w*sizeof(int));
                for (x = 0; x < w; x++) {
                    ptr[x+i*w+x*k*w] = 1;
                }
            } else {
                memcpy(ptr, bitmatrix+k*w*w*(row_ids[i]-k), k*w*w*sizeof(int));
            }
            ptr += (k*w*w);
        }
        inverse = talloc(int, k*k*w*w);
        if (!inverse) {
            free(row_ids);
            free(ind_to_row);
            free(decoding_matrix);
            return NULL;
        }
        jerasure_invert_bitmatrix(decoding_matrix, inverse, k*w);

        /*    printf("\nMatrix to invert\n");
              jerasure_print_bitmatrix(decoding_matrix, k*w, k*w, w);
              printf("\n");
              printf("\nInverse\n");
              jerasure_print_bitmatrix(inverse, k*w, k*w, w);
              printf("\n"); */

        free(decoding_matrix);
        ptr = real_decoding_matrix;
        if (fix_all_data) {
            for (i = 0; i < ddf; i++) {
                memcpy(ptr, inverse+k*w*w*row_ids[k+i], sizeof(int)*k*w*w);
                ptr += (k*w*w);
            }
        } else {
            for (i = 0; i < data_fix_size; i++) {
                memcpy(ptr, inverse+k*w*w*data_fix[i], sizeof(int)*k*w*w);
                ptr += (k*w*w);
            }
        }
        free(inverse);
    }

    /* Next, here comes the hard part.  For each coding node that needs
       to be decoded, you start by putting its rows of the distribution
       matrix into the decoding matrix.  If there were no failed data
       nodes, then you're done.  However, if there have been failed
       data nodes, then you need to modify the columns that correspond
       to the data nodes.  You do that by first zeroing them.  Then
       whereever there is a one in the distribution matrix, you XOR
       in the corresponding row from the failed data node's entry in
       the decoding matrix.  The whole process kind of makes my head
       spin, but it works.
       */

    //for (x = 0; x < cdf; x++) {
    for (x = 0; x < code_fix_size; x++) {
        //drive = row_ids[x+ddf+k]-k;
        drive = code_fix[x] - k;
        ptr = real_decoding_matrix + k*w*w*(ddf+x);
        memcpy(ptr, bitmatrix+drive*k*w*w, sizeof(int)*k*w*w);

        for (i = 0; i < k; i++) {
            if (row_ids[i] != i) {
                for (j = 0; j < w; j++) {
                    bzero(ptr+j*k*w+i*w, sizeof(int)*w);
                }
            }
        }

        // There's the yucky part

        index = drive*k*w*w;
        for (i = 0; i < k; i++) {
            if (row_ids[i] != i) {
                b1 = real_decoding_matrix+(ind_to_row[i]-k)*k*w*w;
                for (j = 0; j < w; j++) {
                    b2 = ptr + j*k*w;
                    for (y = 0; y < w; y++) {
                        if (bitmatrix[index+j*k*w+i*w+y]) {
                            for (z = 0; z < k*w; z++) {
                                b2[z] = b2[z] ^ b1[z+y*k*w];
                            }
                        }
                    }
                }
            }
        }
    }

    /*
       printf("\n\nReal Decoding Matrix\n\n");
       jerasure_print_bitmatrix(real_decoding_matrix, (ddf+cdf)*w, k*w, w);
       printf("\n"); */
    if (smart) {
        schedule = jerasure_smart_bitmatrix_to_schedule(k, decoding_matrix_size, w, real_decoding_matrix);
    } else {
        schedule = jerasure_dumb_bitmatrix_to_schedule(k, decoding_matrix_size, w, real_decoding_matrix);
    }
    free(row_ids);
    free(ind_to_row);
    free(real_decoding_matrix);
    return schedule;
}
