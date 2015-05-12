#!/bin/sh

# /bin/sh on Solaris is not a POSIX compatible shell, but /usr/bin/ksh is.
if [ `uname -s` = 'SunOS' -a "${POSIX_SHELL}" != "true" ]; then
    POSIX_SHELL="true"
    export POSIX_SHELL
    exec /usr/bin/ksh $0 $@
fi
unset POSIX_SHELL # clear it so if we invoke other scripts, they run as ksh as well

set -e

if [ `basename $PWD` != "c_src" ]; then
    # originally "pushd c_src" of bash
    # but no need to use directory stack push here
    cd c_src
fi

BASEDIR="$PWD"


# detecting gmake and if exists use it
# if not use make
# (code from github.com/tuncer/re2/c_src/build_deps.sh
which gmake 1>/dev/null 2>/dev/null && MAKE=gmake
MAKE=${MAKE:-make}

# Changed "make" to $MAKE

case "$1" in
    clean)
        rm -rf jerasure
        rm -rf gf-complete
        ;;

    get-deps)
        export GFP=`pwd`"/gf-complete"
        if [ ! -d gf-complete/src/.libs ]; then
            git clone http://lab.jerasure.org/jerasure/gf-complete.git
            cd gf-complete
            ./autogen.sh
            ./configure
			$MAKE
            cd ..
        fi
        if [ ! -d jerasure/src/.libs ]; then
            git clone http://lab.jerasure.org/jerasure/jerasure.git
            cd jerasure
            autoreconf --install
            ./configure LDFLAGS=-L$GFP/src/.libs/ CPPFLAGS=-I$GFP/include
			$MAKE
            cd ..
        fi
        ;;

    *)
        export GFP=`pwd`"/gf-complete"
        if [ ! -d gf-complete/src/.libs ]; then
            git clone http://lab.jerasure.org/jerasure/gf-complete.git
            cd gf-complete
            ./autogen.sh
			./configure
            $MAKE
            cd ..
        fi
        if [ ! -d jerasure/src/.libs ]; then
            git clone http://lab.jerasure.org/jerasure/jerasure.git
            cd jerasure
            autoreconf --install
            ./configure LDFLAGS=-L$GFP/src/.libs/ CPPFLAGS=-I$GFP/include
            $MAKE
            cd ..
        fi
        ;;
esac
