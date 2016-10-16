#include <stdlib.h>
#include "HashMap.h"
#include <math.h>

typedef enum {False, True} bool;

typedef int (*cmpFn)(const void * a, const void * b);

typedef struct _HeapNode{
    int degree;
    bool marked;
    void * key;
    struct _HeapNode * left, * right, * parent, * child;
} HeapNode;

typedef struct{
    int size;
    cmpFn cmp;
    HeapNode * min;
    /* HashMap * key2node; */
} FibHeap;

FibHeap * makeHeap(cmpFn cmp/* , eqFn cSucks, hashFn hash */){
    FibHeap * ret = (FibHeap *) malloc(sizeof(FibHeap));
    /* ret->key2node = createMap(10, 0.75, hash, cSucks); */
    ret->cmp = cmp;
    ret->size = 0;
    ret->min = NULL;
    return ret;
}

static HeapNode * makeNode(void * key){
    HeapNode * ret = (HeapNode *) malloc(sizeof(HeapNode));
    ret->degree = 0;
    ret->marked = False;
    ret->left = ret->right = ret->parent = ret->child = NULL;
    return ret;
}

int addToHeap(FibHeap * heap, void * key){
    /* if(getKey(heap->key2node, key) != NULL) */
    /*     return 0; */

    HeapNode * addend = makeNode(key);
    if(heap->min == NULL){
        heap->min = addend;
        addend->right = addend->left = addend;
    }else{
        addend->right = heap->min->right;
        addend->left = heap->min;
        heap->min->right = addend;
        addend->right->left = addend;
        if(heap->cmp(key, heap->min->key) < 0){
            heap->min = addend;
        }
    }
    heap->size++;
    /* insert(heap->key2node, key, addend); */
    return 1;
}

FibHeap * mergeHeaps(FibHeap * left, FibHeap * right){
    /* HashMap * addend, * augend; */
    /* if(left->size < right->size){ */
    /*     addend = left->key2node; */
    /*     augend = right->key2node; */
    /* }else{ */
    /*     addend = right->key2node; */
    /*     augend = left->key2node; */
    /* } */
    if(left->cmp != right->cmp) return NULL;

    HeapNode * rr = left->min->right, * ll = right->min->left;
    left->min->right = right->min;
    right->min->left = left->min;
    ll->right = rr;
    rr->left = ll;

    FibHeap * temp = makeHeap(left->cmp);
    temp->size = left->size + right->size;
    if(left->cmp(left->min, right->min) < 0){
        temp->min = left->min;
    }else{
        temp->min = right->min;
    }

    free(left); free(right);
    return temp;
}

void consolidate(FibHeap * heap){
    HeapNode ** arr = (HeapNode **) calloc((int) log2(heap->size) + 1, sizeof(HeapNode *));

    HeapNode * tmp = heap->min;
    do{
        while(arr[tmp->degree] != NULL){
            HeapNode * mn, * ot;
            if(heap->cmp(arr[tmp->degree]->key, tmp->key) < 0){
                mn = arr[tmp->degree];
                ot = tmp;
            }else{
                mn = tmp;
                ot = arr[tmp->degree];
            }

            arr[tmp->degree] = NULL;
            ot->right->left = ot->left;
            ot->left->right = ot->right;

            ot->parent = mn;
            ot->marked = False;
            if(mn->child == NULL){
                mn->child = ot;
                ot->left = ot->right = ot;
            }else{
                ot->right = mn->child->right;
                ot->left = mn->child;
                mn->child->right = ot;
                ot->right->left = ot;
                if(heap->cmp(ot->key, mn->child->key) < 0)
                    mn->child = ot;
            }
            mn->degree++;
            tmp = mn;
        }
        arr[tmp->degree] = tmp;

        tmp = tmp->right;
    }while(tmp != heap->min);

    HeapNode * tMin = heap->min, * iter;
    for(iter = tMin->right; iter != heap->min; iter = iter->right){
        if(heap->cmp(iter->key, tMin->key) < 0)
            tMin = iter;
    }
    heap->min = tMin;
}

void * deleteMin(FibHeap * heap){
    if(heap->size == 0)
        return NULL;
    else if(heap->size == 1){
        void * mk = heap->min->key;
        free(heap->min);
        heap->size = 0;
        return mk;
    }

    heap->size--;
    HeapNode * temp = heap->min;
    heap->min = temp->right;

    temp->left->right = temp->right;
    temp->right->left = temp->left;

    void * key = temp->key;
    free(temp);
    consolidate(heap);
    return key;
}
