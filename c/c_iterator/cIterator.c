#include "cIterator.h"
#include <stdlib.h>

IterNode * makeIterator(void * state, void * value, NextMaker next){
        IterNode * in = (IterNode *) malloc(sizeof(IterNode));
        in->state = state;
        in->value = value;
        in->prev = NULL;
        in->getNextValue = next;
        in->next = NULL
        return in;
}

void freeIterator(IterNode * list, DeallocFn state, DeallocFn value){
        state(list->state);
        value(list->value);
        if(list->next != NULL)
                free(list->next);
        free(list);
}

void defaultFreeIter(IterNode * list){
        freeIterator(list, free, free);
}

IterNode * getNext(IterNode * this){
        if(this->next == NULL){
                this->next = this->getNextValue(this);
                this->next->prev = this;
        }
        return this->next;
}

IterNode * destGetNext(IterNode * this, DeallocFn state, DeallocFn value){
        IterNode * temp = this->getNextValue(this);
        temp->prev = NULL;
        this->next = NULL;
        if(this->prev != NULL)
                this->prev->next = NULL;
        freeIterator(this, state, value);
        return temp;
}

int hasNext(IterNode * this){
        IterNode * next = getNext(this);
        int rv = next != NULL;
        return rv;
}

typedef struct{
        IterNode * currPtr;
        void * (* mapFn)(void *);
} MapState;

IterNode * mapNextVal(IterNode * this){
        MapState * state = (MapState *) this->state;
        MapState * nextState = (MapState *) malloc(sizeof(MapState));
        nextState->currPtr = getNext(state->currPtr);
        if(nextState->currPtr == NULL){
                free(nextState);
                return NULL;
        }
        nextState->mapFn = state->mapFn;
        return makeIterator(nextState, 
                        state->mapFn(state->currPtr->value), 
                        &mapNextVal);
}

IterNode * map(IterNode * source, void * (* mapFn)(void *)){
        MapState * init = (MapState *) malloc(sizeof(MapState));
        init->currPtr = source;
        init->mapFn = mapFn;
        return makeIterator(init, mapFn(source->value), &mapNextVal);
}

void * reduce(IterNode * source, void * initV, void * (* redFn)(void *, void *)){
        for(IterNode * tmp = source; tmp != NULL; tmp = getNext(tmp)){
                initV = redFn(initV, tmp->value);
        }
        return initV;
}

typedef struct{
        IterNode * currPtr;
        int (* pred)(void *);
} FilterState;

IterNode * nextFilterNode(IterNode * this){
        FilterState * curr = (FilterState *) this->state;
        FilterState * next = (FilterState *) malloc(sizeof(FilterState));
        for(next->currPtr = getNext(curr->currPtr);
                        next->currPtr != NULL && 
                                !curr->pred(next->currPtr->value);
                        next->currPtr = getNext(next->currPtr));
        if(next->currPtr == NULL){
                free(next);
                return NULL;
        }
        next->pred = curr->pred;
        return makeIterator(next, next->currPtr->value, &nextFilterNode);
}

IterNode * filter(IterNode * source, int (* pred)(void *)){
        while(source != NULL && !pred(source->value))
                source = getNext(source);
        FilterState * init = (FilterState *) malloc(sizeof(FilterState));
        init->currPtr = source;
        init->pred = pred;
        return makeIterator(init, source->value, &nextFilterNode);
}

IterNode * nextIntFromBy(IterNode * this){
        int * state = (int *) this->state;
        int curr = * (int *) (this->value);
        int * nextCurr = (int *) malloc(sizeof(int));
        *nextCurr += *state;
        return makeIterator(state, nextCurr, &nextIntFromBy);
}

IterNode * intsFromBy(int from, int by){
        int * state = malloc(sizeof(int));
        *state = by;
        int * val = malloc(sizeof(int));
        *val = from;
        return makeIterator(state, val, &nextIntFromBy);
}
