#include "common.h"

size_t roundTo(size_t numToRound, size_t multiple) {
    if (multiple == 0) {
        return numToRound;
    }

    size_t remainder = numToRound % multiple;
    if (remainder == 0)
        return numToRound;
    return numToRound + multiple - remainder;
}
