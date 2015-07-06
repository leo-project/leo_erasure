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

/* is_prime returns 1 if number if prime, 0 if not prime */
int is_prime(int w) {
	int prime55[] = {2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,
	    73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,
		    181,191,193,197,199,211,223,227,229,233,239,241,251,257};
	int i;
	for (i = 0; i < 55; i++) {
		if (w%prime55[i] == 0) {
			if (w == prime55[i]) return 1;
			else { return 0; }
		}
	}
    return 1;
}
