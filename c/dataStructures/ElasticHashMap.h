#ifndef __ElasticHashMap__header
#define __ElasticHashMap__header

typedef unsigned int (*HashFn) (const void * in);
typedef unsigned int (*EqFn)(const void * obj1, const void * obj2);
typedef void (*Deallocator) (void * obj);

typedef struct {
    HashFn hash;
    EqFn eq;

    // n, and which of the A_n we're allowed to probe until.
    unsigned long int capacity;
    // Batch endpoint will be a running total while the length gives the size of the latest batch.
    unsigned long int batch_endpoint_, next_batch_length_;

    unsigned long int * subarray_loads_;

    void ** map_;
} ElasticHashMap;

// Allocates an ElasticHashMap.
// TODO: make a version that only allocates the sub arrays?
ElasticHashMap * ElasticMap_Initialize(HashFn hash, EqFn eq, unsigned long int capacity);

void ElasticMap_Free(ElasticHashMap* map);

typedef enum InsertionStatus {
    ElasticMap_InsertionStatus_INSERTED,
    ElasticMap_InsertionStatus_EQUAL_ELEMENT_FOUND,
    ElasticMap_InsertionStatus_FULL,
} ElasticMap_InsertionStatus;

ElasticMap_InsertionStatus ElasticMap_Insert(ElasticHashMap* map, void* key);

#endif
