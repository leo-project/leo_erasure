#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "jerasure_mod.h"

#define talloc(type, num) (type *) malloc(sizeof(type)*(num))

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
  
  /* Finally, re-encode any erased coding devices */
  /* Don't need

  for (i = 0; i < m; i++) {
    if (erased[k+i]) {
      jerasure_matrix_dotprod(k, w, matrix+(i*k), NULL, i+k, data_ptrs, coding_ptrs, size);
    }
  }
  */

  free(erased);
  if (dm_ids != NULL) free(dm_ids);
  if (decoding_matrix != NULL) free(decoding_matrix);

  return 0;
}


