#ifndef __cIterator__incl
#define __cIterator__incl

struct __IterNode_def{
        void * state;
        void * value;
        struct __IterNode_def * (* getNextValue)(struct __IterNode_def * this);
        struct __IterNode_def * next;
        struct __IterNode_def * prev;
};

typedef struct __IterNode_def IterNode;
typedef IterNode * (* NextMaker) (IterNode * this);
typedef void (* DeallocFn) (void *);

IterNode * makeIterator(void * state, void * value, NextMaker next);
void freeIterator(IterNode * list, DeallocFn state, DeallocFn value);
void defaultFreeIter(IterNode * list);

IterNode * getNext(IterNode * this);
IterNode * destGetNext(IterNode * this, DeallocFn state, DeallocFn value);
int hasNext(IterNode * this);

IterNode * map(IterNode * source, void * (* mapfn)(void *));
void * reduce(IterNode * source, void * initV, void * (* redFn)(void *, void *));

IterNode * filter(IterNode * source, int (* pred)(void *));

IterNode * intsFromBy(int from, int by);
#endif
