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
  * `encode/4` (Bin, BinSize, Coding, Coding_Parameters `{k,m,w}`)
  * `decode/5` (Blocks, AvailableBlocksIDs, FileSize, Coding, Coding, Coding_Parameters `{k,m,w}`)
* Helper Methods
  * `encode_file/3` (File_Name, Coding, Coding_Parameters `{k,m,w}`)
    * Encode the file and store the data/code blocks at `block/`
  * `decode_file/4` (File_Name, File_Size, Coding, Coding_Parameters `{k,m,w}`)
    * Decode from the blocks in `block/` to reconstruct the file
  * `benchmark_encode/3` (Total_Size_MB, One_Size_MB, Coding, Coding_Parameters `{k,m,w}`)
    * Benchmark the encoding speed
