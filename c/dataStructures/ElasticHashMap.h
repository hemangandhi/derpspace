#ifndef __ElasticHashMap__header
#define __ElasticHashMap__header

typedef unsigned long int (*HashFn) (const void * in);
typedef unsigned int (*EqFn)(const void * obj1, const void * obj2);
typedef void (*Deallocator) (void * obj);

typedef struct {
    HashFn hash;
    EqFn eq;

    // Load factor's exponent (delta = 2^(-load_factor)).
    unsigned char load_factor;

    // n, and which of the A_n we're allowed to probe until.
    unsigned long int capacity;
    unsigned long int current_batch_number_, current_batch_inserts_;

    unsigned long int * subarray_loads_;
    void ** current_batch_subarray_start_;
    void ** map_;
} ElasticHashMap;

// Allocates an ElasticHashMap.
// TODO: make a version that only allocates the sub arrays?
ElasticHashMap * ElasticMap_Initialize(HashFn hash, EqFn eq, unsigned long int capacity, unsigned char load_factor);

void ElasticMap_Free(ElasticHashMap* map, Deallocator deallocator);

typedef enum InsertionStatus {
    ElasticMap_InsertionStatus_INSERTED,
    ElasticMap_InsertionStatus_EQUAL_ELEMENT_FOUND,
    ElasticMap_InsertionStatus_FULL,
} ElasticMap_InsertionStatus;

ElasticMap_InsertionStatus ElasticMap_Insert(ElasticHashMap* map, void* key);

#endif
