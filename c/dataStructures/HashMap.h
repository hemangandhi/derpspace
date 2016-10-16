#ifndef __HashMap__header
#define __HashMap__header

typedef int (*hashFn) (const void * in);
typedef int (*eqFn)(const void * obj1, const void * obj2);
typedef void (*deallocator) (void * obj);

struct __Node__def{
        void * key, * value;
        struct __Node__def * next;
};

typedef struct __Node__def Node;

typedef struct {
        hashFn hash;
        eqFn eq;
        Node ** nds;
        int listSize, mapSize;
        float loadFactor;
} HashMap;


HashMap * createMap(int initSize, float lfa, hashFn hs, eqFn e);

void freeNode(Node * nd, deallocator freeKey, deallocator freeVal);

void freeMap(HashMap * map, deallocator freeKey, deallocator freeVal);

void defaultFreeMap(HashMap * map);

void * removeKey(HashMap * map, const void * key, deallocator del);

void * defaultRemoveKey(HashMap * map, const void * key);

void rehash(HashMap * map);

void * insert(HashMap * map, void * key, void * val);

void * getKey(HashMap * map, const void * key);
#endif
