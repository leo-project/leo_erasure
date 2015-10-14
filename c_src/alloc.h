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
