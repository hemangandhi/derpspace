#ifndef __ElasticHashMap__header
#define __ElasticHashMap__header

typedef unsigned int (*HashFn) (const void * in);
typedef unsigned int (*EqFn)(const void * obj1, const void * obj2);
typedef void (*Deallocator) (void * obj);

typedef struct {
    HashFn hash;
    EqFn eq;

    // n, and which of the A_n we're allowed to probe until.
    unsigned int capacity, batch_;

    void ** map_;
} ElasticHashMap;

#endif
