leo_jerasure
===========

[![Build Status](https://secure.travis-ci.org/leo-project/leo_jerasure.png?branch=develop)](http://travis-ci.org/leo-project/leo_jerasure)

Overview
--------
* "leo_jerasure" is a Erlang binding for the open sourced Erasure Coding library "Jerasure"
  * Coding Supported: Reed-Solomon Code (Vandermonde/Cauchy), Liberation Code
* "leo_jerasure" uses [rebar](https://github.com/basho/rebar) build system. Makefile so that simply running "make" at the top level should work.
* Object would be encoded into `{k + m}` blocks, any `{k}` blocks could be used to decode back

Exported Method
--------
* Encode / Decode
  * `encode/4` (Bin, BinSize, Coding, CodingParameters `{k,m,w}`)
    * Encode a binary into blocks
  * `decode/4` ([{Block, BlockID}], FileSize, Coding, CodingParameters `{k,m,w}`)
    * Decode blocks back to original binary, interface with [{Block, BlockID}]
  * `decode/5` ([Block], [BlockID], FileSize, Coding, CodingParameters `{k,m,w}`)
    * Decode blocks back to original binary, interface with [Block] and [BlockID]
* Block Rebuild
  * `repair_one/4` ([{Block, BlockID}], RepairID, Coding, CodingParameters `{k,m,w}`)
    * Repair the block specified with RepairID, interface with [{Block, BlockID}] 
  * `repair_one/5` ([Block], [BlockID], RepairID, Coding, CodingParameters `{k,m,w}`)
    * Repair the block specified with RepairID, interface with [Block] and [BlockID] 
* Helper Methods
  * `encode_file/3` (FileName, Coding, CodingParameters `{k,m,w}`)
    * Encode the file and store the data/code blocks at `block/`
  * `decode_file/4` (FileName, FileSize, Coding, CodingParameters `{k,m,w}`)
    * Decode from the blocks in `block/` to reconstruct the file
  * `benchmark_encode/3` (TotalSizeMB, RoundSizeMB, Coding, CodingParameters `{k,m,w}`)
    * Benchmark the encoding speed

Note
--------
* [Block] and [BlockID] are one-to-one mapped accordingly, and assumed to be sorted with BlockID (Ascending)

Dependencies
--------
* "leo_jerasure" : Erlang 16/17.x
* "Jerasure" : Automake 1.13+, Autoconf 2.65+, Libtool
  * For Ubuntu / Debian
  ```
  sudo apt-get install git automake autoconf libtool build-essential
  ```
