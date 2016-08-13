#ifndef __ll_incl
#define __ll_incl
#include <stdlib.h>

struct __Node{
        void * val;
        struct __Node * next;
};

typedef struct __Node Node;
typedef void (* DeallocFn)(void *);

Node * newNode(void * val, Node * next){
        Node * retVal = (Node *) malloc(sizeof(Node));
        retVal->val = val;
        retVal->next = next;
        return retVal;
}

void delNode(Node * list, DeallocFn valFreer){
        if(list->next != NULL)
                delNode(list->next, valFreer);
        valFreer(list->val);
        free(list);
}

void defaultDelNode(Node * list){
        delNode(list, &free);
}

// <ret type> (* name) (<arg1 type...>)
// a->b === (*a).b
#endif
