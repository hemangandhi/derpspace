#include "ElasticHashMap.h"

static unsigned int _index_of_highest_bit(unsigned int n) {
    if (n <= 2) return n;
    unsigned int acc = 1;
    if ((n >> 16) > 0) {
        acc += 16;
        n >>= 16;
    }
    if ((n >> 8) > 0) {
        acc += 8;
        n >>= 8;
    }
    if ((n >> 4) > 0) {
        acc += 4;
        n >>= 4;
    }
    if ((n >> 2) > 0) {
        acc += 2;
        n >>= 2;
    }
    if ((n >> 1) > 0) {
        acc += 1;
        n >>= 1;
    }
    return acc;
}

static unsigned int _phi(unsigned int i, unsigned int j) {

}

#ifdef UNIT_TEST

#include <stdio.h>

int main(int argc, char** argv) {
    printf("sizeof(unsigned int) = %lx\n", sizeof(unsigned int));
    printf("Testing highest bit.\n");
    for(unsigned int i = 0; i < 16; i++) {
        printf("Highest bit of %u is %u.\n", i * 32, _index_of_highest_bit(i * 32));
    }
}
#endif
