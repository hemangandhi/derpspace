#ifndef __UpdateableHeap__header
#define __UpdateableHeap__header
#include <stdlib.h>
#include <stdio.h>
#include "HashMap.c"


typedef int (*comparator) (const void * obj1, const void * obj2);

typedef struct{
        comparator cmp;
        HashMap * toInd;
        void ** data;
        int size, listSize;
} UpdateableHeap;

UpdateableHeap * createHeap(int initSize, hashFn hs, eqFn e, comparator c){
        UpdateableHeap * ret = (UpdateableHeap *) malloc(sizeof(UpdateableHeap));
        ret->size = 0;
        ret->listSize = initSize;
 
        ret->toInd = createMap(initSize, 0.75f, hs, e);
        ret->data = malloc(initSize*sizeof(void *));
        ret->cmp = c;
        return ret;
}

void freeHeap(UpdateableHeap * heap){
        defaultFreeMap(heap->toInd);
        free(heap->data);
        free(heap);
}

void swapInHeap(UpdateableHeap * heap, int v1, int v2){
        if(v1 == v2) return;

        void * atV1 = heap->data[v1];
        void * atV2 = heap->data[v2];
        int * v1Ptr = (int *) malloc(sizeof(int));
        *v1Ptr = v1;
        int * v2Ptr = (int *) malloc(sizeof(int));
        *v2Ptr = v2;
 //       printf("sih::before insert in hash (swapping %d and %d)\n", v1, v2);
        int * o1 = insert(heap->toInd, atV1, v2Ptr);
   //     printf("Inserted w/ o1\n");
        int * o2 = insert(heap->toInd, atV2, v1Ptr);
        //printf("sih::before free (o1 = %d and o2 = %d)\n", *o1, *o2);
        free(o1); free(o2);
     //   printf("swapInHeap::freed o1 and o2.\n");
        heap->data[v1] = atV2;
        heap->data[v2] = atV1;
}

void siftUpOrDown(UpdateableHeap * heap, int idx){
        int i, j;
        for(i = idx, j = (i - 1)/2; j >= 0; i = j, j = (i - 1)/2){
                if(heap->cmp(heap->data[i], heap->data[j]) > 0){
                        swapInHeap(heap, i, j);
                }else break;
        }

   //     printf("Sifted up!\n");

        int k;
        for(j = 2*i + 1, k = 2*i + 2; j < heap->size; j = 2*i + 1, k = 2*i + 2){
                int sm = (k >= heap->size || heap->cmp(heap->data[j], heap->data[k]) > 0)? j:k;
                if(heap->cmp(heap->data[sm], heap->data[i]) > 0){
                        swapInHeap(heap, i, sm);
                        i = sm;
                }else break;
        }

 //       printf("Sifted down!\n");
}

void addToHeap(UpdateableHeap * heap, void * val){
        if(heap->size == heap->listSize){
                heap->listSize *= 2;
                heap->data = realloc(heap->data, heap->listSize*sizeof(void *));
        }
        heap->size++;
        heap->data[heap->size - 1] = val;
        int * t = malloc(sizeof(int));
        *t = heap->size - 1;
        insert(heap->toInd, val, t);
 //       printf("addToHeap::Inserted into hashmap.\n");
        siftUpOrDown(heap, heap->size - 1);
}

void * deleteMin(UpdateableHeap * heap){
        if(heap->size == 0)
                return NULL;

        swapInHeap(heap, 0, heap->size - 1);
        heap->size--;
        siftUpOrDown(heap, 0);
        return heap->data[heap->size];
}

void updateHeap(UpdateableHeap * heap, void * old, void * new){
        int * v = (int *) defaultRemoveKey(heap->toInd, old);
        insert(heap->toInd, new, v);
        heap->data[*v] = new;
        siftUpOrDown(heap, *v);
}

int heapContains(UpdateableHeap * heap, void * key){
        return getKey(heap->toInd, key) != NULL;
}

int cmpInt(void * x, void * y){
        return (* (int *) y - * (int *) x);
}

/*
int main(int argc, char ** argv){
        UpdateableHeap * h = createHeap(10, &hsInt, &eqInt, &cmpInt);
        int * v;
        for(int i = 0; i < 10; i++){
                v = malloc(sizeof(int));
                *v = 9 - i;
                addToHeap(h, v);
                printf("Added %d to the heap...\n", *v);
        }

        int * w = malloc(sizeof(int));
        *w = 11;
        updateHeap(h, v, w);

        for(int i = 0; i < 10; i++){
                int * m = (int *) deleteMin(h);
                printf("Min : %d\n", *m);
                free(m);
        }

        freeHeap(h);
}
*/
#endif
