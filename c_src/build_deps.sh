#!/bin/bash

test `basename $PWD` != "c_src" && cd c_src

cd ../deps/gfcomplete
export GFP=`pwd`
if [ ! -f src/.libs/libgf_complete.so ]; then
    ./autogen.sh
    ./configure
    make -j4
fi
cd ../jerasure
if [ ! -f src/.libs/libJerasure.so ]; then
    autoreconf --install
    ./configure LDFLAGS=-L$GFP/src/.libs/ CPPFLAGS=-I$GFP/include
    make -j4
fi

cd $PWD
