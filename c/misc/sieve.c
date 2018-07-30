#include <stdio.h>
#include <errno.h>
#include <stdlib.h>

typedef struct IntNode_ {
    int val;
    struct IntNode_ * next;
} IntNode;

void freeIntNode(IntNode * n){
    for(IntNode * t = n; n != NULL; n = t){
        t = n->next;
        free(n);
    }
}

IntNode * addToSieve(IntNode * sieve, int n){
    int init = 2;
    for(; n > 0; init++, n--){
        IntNode * prev = NULL, * c;
        for(c = sieve; c != NULL && init % c->val != 0; c = c->next) prev = c;

        if(c == NULL){
            if(prev == NULL){
                sieve = (IntNode *) malloc(sizeof(IntNode));
                if(sieve == NULL){
                    return NULL;
                }
                sieve->val = init;
                sieve->next = NULL;
            }else{
                prev->next = (IntNode *) malloc(sizeof(IntNode));
                if(prev->next == NULL){
                    freeIntNode(sieve);
                    return NULL;
                }
                prev->next->val = init;
                prev->next->next = NULL;
            }
        }else{
            n++;
        }
    }

    return sieve;
}

int main(int argc, char ** argv){
    if(argc != 2){
        printf("USAGE: %s num primes", *argv);
        return -1;
    }

    int v = atoi(argv[1]);
    if(v < 1){
        printf("The first %d primes: \n", v);
        return 0;
    }else{
        IntNode * s = addToSieve(NULL, v);
        if(s == NULL){
            perror("Could not find primes");
            return -1;
        }
        printf("The first %d primes: \n", v);
        for(IntNode * t = s; t != NULL; t = t->next) printf("%d\n", t->val);
        freeIntNode(s);
        return 0;
    }
}
