#include "ElasticHashMap.h"

#include <stdlib.h>

static unsigned short IndexOfHighestBit_(unsigned short n) {
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

// Cries in optimizing without templates: C++ or Rust (or probably Zig) could just
// know the sizeof(n) at compile-time and stamp out pretty little loops per type.
static unsigned long int IndexOfHighestBitLong_(unsigned long int n) {
    if (n <= 2) return n;
    unsigned long int acc = 1;
    for (unsigned char offset = 32; offset > 1; offset >>= 1) {
        if ((n >> offset) > 0) {
            acc += offset;
            n >>= offset;
        }
    }
    if ((n >> 1) > 0) {
        acc += 1;
        n >>= 1;
    }
    return acc;
}



static unsigned long int Phi_(unsigned short i, unsigned short j) {
    unsigned short top_j_bit = IndexOfHighestBit_(j);
    unsigned short top_i_bit = IndexOfHighestBit_(i);
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

ElasticHashMap * ElasticMap_Initialize(HashFn hash, EqFn eq, unsigned long int capacity) {
    void ** map_array = (void **) calloc(capacity, sizeof(void *));
    if (map_array == NULL) {
        goto err_null;
    }
    unsigned long int * subarray_loads = (unsigned long int *) calloc(
            sizeof(unsigned long int), IndexOfHighestBitLong_(capacity));
    if (subarray_loads == NULL) {
        goto err_array;
    }
    ElasticHashMap * map = (ElasticHashMap *) malloc(sizeof(ElasticHashMap));
    if (map == NULL) {
        goto err_loads;
    }
    map->hash = hash;
    map->eq = eq;
    map->capacity = capacity;
    map->batch_endpoint_ = map->capacity / 2;
    map->next_batch_length_ = map->capacity / 4;
    map->map_ = map_array;
    map->subarray_loads_ = subarray_loads;
    return map;

    // RAII or defer are so much better.
err_loads:
    free(subarray_loads);
err_array:
    free(map_array);
err_null:
    return NULL;
}

void ElasticMap_Free(ElasticHashMap* map) {
    free(map->subarray_loads_);
    free(map->map_);
    free(map);
}

#ifdef UNIT_TEST

#include <stdio.h>

int main(int argc, char** argv) {
#ifdef TEST_INDEX_OF_BIT
    printf("sizeof(unsigned short) = %lx\n", sizeof(unsigned short));
    printf("sizeof(unsigned long int) = %lx\n", sizeof(unsigned long int));
    printf("Testing highest bit: shorts.\n");
    for(unsigned short i = 0; i < 16; i++) {
        printf("Highest bit of %u is %u.\n", i * 32, IndexOfHighestBit_(i * 32));
    }
    printf("Testing highest bit: long.\n");
    for(unsigned long int i = 0; i < 16; i++) {
        printf("Highest bit of %lu is %lu.\n", i * 32, IndexOfHighestBitLong_(i * 32));
        printf("Highest bit of %lu is %lu.\n", i * 3002, IndexOfHighestBitLong_(i * 3002));
    }
#endif
#ifdef TEST_PHI_FUNCTION
    for (unsigned short i = 0; i < 3; i++) {
        for (unsigned short j = 0; j < 3; j++) {
            printf("phi(%x, %x) = %lx\n", i, j, Phi_(i, j));
        }
    }
#endif
}
#endif
