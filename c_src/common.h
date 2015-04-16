#ifndef __COMMON_H__
#define __COMMON_H__

#include <stddef.h>

typedef struct {
    size_t size;
    unsigned char* data;
} BlockEntry;

size_t roundTo(size_t numToRound, size_t multiple) ;
#endif
