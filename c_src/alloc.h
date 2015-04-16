#ifndef __ALLOC_H__
#define __ALLOC_H__
#ifdef NIF
#include "erl_nif.h"
static void* (*alloc) (size_t) = &enif_alloc;
static void (*dealloc) (void*) = &enif_free;
#else
#include <stdlib.h>
static void* (*alloc) (size_t) = &malloc;
static void (*dealloc) (void*) = &free;
#endif
#endif
