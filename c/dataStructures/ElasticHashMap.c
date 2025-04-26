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

ElasticHashMap * ElasticMap_Initialize(HashFn hash, EqFn eq, unsigned long int capacity, unsigned char load_factor) {
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
    map->load_factor = load_factor;
    map->map_ = map_array;
    map->current_batch_number_ = 0;
    map->current_batch_inserts_ = 0;
    map->current_batch_subarray_start_ = map_array;
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

void ElasticMap_Free(ElasticHashMap* map, Deallocator deallocator) {
    free(map->subarray_loads_);
    for (void ** datum = map->map_; datum < map->map_ + map->capacity; datum++) {
        deallocator(*datum);
    }
    free(map->map_);
    free(map);
}

static unsigned long int ProbeOfHash_(
    unsigned long int hash,
    unsigned long int batch_size,
    // TODO: what to do about the fact that these get truncated?
    unsigned long int i,
    unsigned long int j
) {
    // TODO: better hash function?
    // It's not like Phi has a good distribution of bits?
    return (hash ^ Phi_(i, j)) % batch_size;
}

ElasticMap_InsertionStatus ElasticMap_Insert(ElasticHashMap* map, void* key) {
    unsigned long int key_hash = map->hash(key);
    // If the first array isn't yet 75% full, we're in the first batch.
    // 75% of first array = 3/4 * capacity / 2, which works out to the below.
    if (*map->subarray_loads_ * 8 <= map->capacity * 3) {
        for (unsigned long int j = 1; j < map->capacity/2; j++) {
            unsigned long int index = ProbeOfHash_(key_hash, map->capacity/2, 1, j);
            if (map->map_[index] == NULL) {
                map->map_[index] = key;
                (*map->subarray_loads_)++;
                return ElasticMap_InsertionStatus_INSERTED;
            } else if (map->eq(key, map->map_[index])) {
                return ElasticMap_InsertionStatus_EQUAL_ELEMENT_FOUND;
            }
        }

        // Probably only happens with capacity = 1 or something ludicrous like that.
        return ElasticMap_InsertionStatus_FULL;
    }

    unsigned long int current_subarray_size = map->capacity >> (map->current_batch_number_ + 1);
    // A1 > 75% full, the first time, so we're merely moving from batch 0 to 1,
    // but the subarrays are still A1 and A2.
    if (map->current_batch_number_ == 0) {
        map->current_batch_number_++;
    // Otherwise, we migth be moving from batch i to batch i + 1.
    } else if (9 * current_subarray_size / 2 < 8 * map->current_batch_inserts_ - 8 * current_subarray_size + (current_subarray_size >> (map->load_factor - 2))) {
        map->current_batch_number_++;
        map->current_batch_inserts_ = 0;
        map->current_batch_subarray_start_ += current_subarray_size;
        current_subarray_size >>= 1;

        if (map->current_batch_subarray_start_ + current_subarray_size >= map->map_ + map->capacity) {
            return ElasticMap_InsertionStatus_FULL;
        }
    }

}

#ifdef UNIT_TEST

#include <stdio.h>

#if defined(TEST_INSERTIONS_INTO_FIRST_HALF)
unsigned long int daftIntHash(const void * p) {
    return *(unsigned long int*) p;
}

unsigned int intEq(const void * p, const void * q) {
    return (*(unsigned long int*) p) == (*(unsigned long int*) q);
}

void doNothingDealloc(void * p) {}

const unsigned long int kInsertions[50] = {
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
    30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
    40, 41, 42, 43, 44, 45, 46, 47, 48, 49
};
#endif

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
#ifdef TEST_INSERTIONS_INTO_FIRST_HALF
    ElasticHashMap * test_map = ElasticMap_Initialize(daftIntHash, intEq, 300, 50);
    if (test_map == NULL) {
        puts("Failed to allocate map.");
        return 1;
    }
    ElasticMap_Free(test_map, doNothingDealloc);
    return 0;
#endif
}
#endif
