#include "ElasticHashMap.h"

static unsigned short _index_of_highest_bit(unsigned short n) {
    if (n <= 2) return n;
    unsigned short acc = 1;
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

static unsigned long int _phi(unsigned short i, unsigned short j) {
    unsigned short top_j_bit = _index_of_highest_bit(j);
    unsigned short top_i_bit = _index_of_highest_bit(i);
    // NOTE: ~0 extends?
    unsigned short j_ones = 0xFFFF >> (8 * sizeof(unsigned short) - top_j_bit);
    unsigned long int result = ((unsigned long) j_ones) << top_j_bit;
    result |= j_ones;
    for (unsigned char bit = top_j_bit; bit > 0; bit--) {
        unsigned char bit_value = j & (0x1 << bit - 1);
        if (bit_value) continue;
        result &= ~(0x1 << (bit * 2));
    }
    result <<= top_i_bit + 1;
    result |= i;
    return result;
}

#ifdef UNIT_TEST

#include <stdio.h>

int main(int argc, char** argv) {
#ifdef TEST_INDEX_OF_BIT
    printf("sizeof(unsigned short) = %lx\n", sizeof(unsigned short));
    printf("Testing highest bit.\n");
    for(unsigned int i = 0; i < 16; i++) {
        printf("Highest bit of %u is %u.\n", i * 32, _index_of_highest_bit(i * 32));
    }
#endif
    for (unsigned short i = 0; i < 3; i++) {
        for (unsigned short j = 0; j < 3; j++) {
            printf("phi(%x, %x) = %lx\n", i, j, _phi(i, j));
        }
    }
}
#endif
