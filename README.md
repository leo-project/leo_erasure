# leo_jerasure

[![Build Status](https://secure.travis-ci.org/leo-project/leo_jerasure.png?branch=develop)](http://travis-ci.org/leo-project/leo_jerasure)

## Overview

* "leo_jerasure" is a Erlang binding for the open sourced Erasure Coding library "Jerasure"
  * Coding Supported: Reed-Solomon Code (Vandermonde/Cauchy), Liberation Code
* "leo_jerasure" uses [rebar](https://github.com/rebar/rebar) build system. Makefile so that simply running "make" at the top level should work.

## Description
* Object would be encoded into `{k + m}` blocks, any `{k}` blocks could be used to decode back
* **K**: The number of data chunks - The number of chunks in which the original object is divided
* **M**: The number of coding chunks - The number of additional chunks computed by leo_jerasure's encoding functions

### Dependencies
* [Erlang 16/17](erlang.org)
* [Automake 1.13+](http://www.gnu.org/software/automake/), [Autoconf 2.65+](http://www.gnu.org/software/autoconf/autoconf.html) and [Libtool](http://www.gnu.org/software/libtool/)

### Installation
#### For Ubuntu 14.04LTS	or higher
```bash
$ sudo apt-get install git automake autoconf libtool build-essential
```

#### For RHEL/CentOS v6/7
```bash
## ------------------------------
## For CentOS/RHEL 6.5/6.6 and 7
## ------------------------------
## 1. You need to install ``autoconf268`` because Leo's erasure-coding lib requires Autoconf 2.65 or higher
$ cd <workspace>/
$ wget http://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.gz
$ tar xvfvz autoconf-2.69.tar.gz
$ cd autoconf-2.69
$ ./configure && make && sudo make install
$ autoconf --version
autoconf (GNU Autoconf) 2.69
Copyright (C) 2012 Free Software Foundation, Inc.
License GPLv3+/Autoconf: GNU GPL version 3 or later
<http://gnu.org/licenses/gpl.html>, <http://gnu.org/licenses/exceptions.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

Written by David J. MacKenzie and Akim Demaille.

## 2. Furthermore, according to the same reason of ``autoconf``, you need to install the latest ``automake`` manually.
$ cd <workspace>/
$ wget http://ftp.gnu.org/gnu/automake/automake-1.13.4.tar.gz
$ tar xzf automake-1.13.4.tar.gz
$ cd automake-1.13.4
$ ./configure && make && sudo make install
$ automake --version
automake (GNU automake) 1.13.4
Copyright (C) 2013 Free Software Foundation, Inc.
License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl-2.0.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

Written by Tom Tromey <tromey@redhat.com>
       and Alexandre Duret-Lutz <adl@gnu.org>.
```

## Data Types
### coding_class()
* coding_class() = vandrs | cauchyrs | liberation
    * vandrs *(default)* - Vandermonde-based Reed-Solomon codes which is based on traditional ``Reed-Solomon codes``
    * cauchyrs - Cauchy-based Reed-Solomon codes which is also based on traditional ``Reed-Solomon codes``
    * liberation - Liberation coding and decoding are based on a bit matrix-vector product very similar to the those used in ``Reed-Solomon coding`` and ``Cauchy Reed-Solomon coding``

### id_with_block()
* id_with_block() = {non_neg_integer(), binary()}


## Exports
### Encoding an object
Encode an object with leo_jerasure's encoding functions

#### encode/2
```erlang
encode({CodingParam_K, CodingParam_M}, Bin) ->
    {ok, IdWithBlockL}|{error, Cause}
```
* Types
    * CodingParam_K = pos_integer()
    * CodingParam_M = pos_integer()
    * Bin = binary()
    * IdWithBlockL = [[id_with_block()](#id_with_block())]
    * Cause = any()

#### encode/3
```erlang
encode(CodingClass, CodingParams, Bin) ->
    {ok, IdWithBlockL}|{error, Cause}
```
* Types
    * CodingClass = [coding_class()](#coding_class())
    * CodingParams = {CodingParam_K, CodingParam_M, CodingParam_W}
    * CodingParam_K = pos_integer()
    * CodingParam_M = pos_integer()
    * CodingParam_W = pos_integer()
    * Bin = binary()
    * IdWithBlockL = [[id_with_block()](#id_with_block())]
    * Cause = any()


### Decoding an object
Decode an object with leo_jerasure's decoding functions

#### decode/3
```erlang
decode({CodingParam_K, CodingParam_M}, IdWithBlockL, ObjSize) ->
    {ok, Bin}|{error, Cause}
```
* Types
    * CodingParam_K = pos_integer()
    * CodingParam_M = pos_integer()
    * IdWithBlockL = [[id_with_block()](#id_with_block())]
    * ObjSize = pos_integer()
    * Bin = binary()
    * Cause = any()

#### decode/4
```erlang
decode(CodingClass, CodingParams, IdWithBlockL, ObjSize) ->
    {ok, Bin}|{error, Cause}
```
* Types
    * CodingClass = [coding_class()](#coding_class())
    * CodingParams = {CodingParam_K, CodingParam_M, CodingParam_W}
    * CodingParam_K = pos_integer()
    * CodingParam_M = pos_integer()
    * CodingParam_W = pos_integer()
    * IdWithBlockL = [[id_with_block()](#id_with_block())]
    * ObjSize = pos_integer()
    * Bin = binary()
    * Cause = any()


### Repairing multi blocks
Repair multiple blocks to retain complete blocks of the object

#### repair/3
```erlang
repair({CodingParam_K, CodingParam_M}, IdWithBlockL) ->
    {ok, IdWithBlockL} | {error, any()}
```
* Types
    * CodingParam_K = pos_integer()
    * CodingParam_M = pos_integer()
    * IdWithBlockL = [[id_with_block()](#id_with_block())]
    * Cause = any()

#### repair/4
```erlang
repair(CodingClass, CodingParams, IdWithBlockL) ->
    {ok, IdWithBlockL} | {error, any()}
```
* Types
    * CodingClass = [coding_class()](#coding_class())
    * CodingParams = {CodingParam_K, CodingParam_M, CodingParam_W}
    * CodingParam_K = pos_integer()
    * CodingParam_M = pos_integer()
    * CodingParam_W = pos_integer()
    * IdWithBlockL = [[id_with_block()](#id_with_block())]
    * Cause = any()

## Sponsors

LeoProject/LeoFS is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and supported by [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).
