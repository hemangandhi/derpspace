#include "cIterator.h"
#include <stdio.h>

int main(){
    IterNode * t = intsFromBy(1, 1);
    for(int i = 0; i < 5; i++){
        printf("%d\n", *(int *)(t->value));
        t = getNext(t);
    }
    defaultFreeIter(t, 1);
}
