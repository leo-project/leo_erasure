leo_jerasure
===========

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
  * `decode/5` (Blocks, AvailableBlocksIDs, FileSize, Coding, CodingParameters `{k,m,w}`)
* Helper Methods
  * `encode_file/3` (FileName, Coding, CodingParameters `{k,m,w}`)
    * Encode the file and store the data/code blocks at `block/`
  * `decode_file/4` (FileName, FileSize, Coding, CodingParameters `{k,m,w}`)
    * Decode from the blocks in `block/` to reconstruct the file
  * `benchmark_encode/3` (TotalSizeMB, RoundSizeMB, Coding, CodingParameters `{k,m,w}`)
    * Benchmark the encoding speed
