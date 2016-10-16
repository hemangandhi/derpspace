#include <stdlib.h>
#include <stdio.h>
#include "HashMap.h"

HashMap * createMap(int initSize, float lfa, hashFn hs, eqFn e){
        HashMap * ret = (HashMap *) malloc(sizeof(HashMap));
        ret->hash = hs;
        ret->listSize = initSize;
        ret->mapSize = 0;
        ret->loadFactor = lfa;
        ret->nds = (Node **) calloc(sizeof(Node *),initSize);
        ret->eq = e;
        return ret;
}

void freeNode(Node * nd, deallocator freeKey, deallocator freeVal){
        if(nd != NULL){
                freeNode(nd->next, freeKey, freeVal);
                free(nd->key);
                free(nd->value);
                free(nd);
        }
}

void freeMap(HashMap * map, deallocator freeKey, deallocator freeVal){
        int i;
        for(i = 0; i < map->listSize; i++)
                freeNode(map->nds[i], freeKey, freeVal);
        free(map->nds);
        free(map);
}

void defaultFreeMap(HashMap * map){
        freeMap(map, free, free);
}

void * removeKey(HashMap * map, const void * key, deallocator del){

        int chk = map->hash(key), ran = 0;
        map->mapSize--;
        Node * v = map->nds[chk % map->listSize], * p;
        for(; v != NULL; p = v, v = v->next, ran++){
                if(map->eq(v->key, key)) break;
        }

        if(v == NULL){
                map->mapSize++;
                return NULL;
        }else if(ran == 0){
                map->nds[chk % map->listSize] = v->next;
                if(key != v->key)
                        del(v->key);
                void * r = v->value;
                free(v);
                return r;
        }else{
                p->next = v->next;
                if(key != v->key)
                        del(v->key);
                void * r = v->value;
                free(v);
                return r;
        }
}

void * defaultRemoveKey(HashMap * map, const void * key){
        return removeKey(map, key, free);
}

void rehash(HashMap * map){
        Node ** newNds = (Node **) calloc(sizeof(Node *) , map->listSize * 2);

        int i;
        for(i = 0; i < map->listSize; i++){
                while(map->nds[i] != NULL){
                        Node * t = map->nds[i];
                        int idx = map->hash(t->key) % (map->listSize * 2);
                        //delete
                        map->nds[i] = t->next;
                        //insert
                        t->next = newNds[idx];
                        newNds[idx] = t;
                }
        }

        free(map->nds);
        map->nds = newNds;
        map->listSize *= 2;
}

void * insert(HashMap * map, void * key, void * val){
        void * ret = defaultRemoveKey(map, key);
        int idx = map->hash(key) % map->listSize;
        Node * new = (Node *) malloc(sizeof(Node));
        new->key = key;
        new->value = val;
        new->next = map->nds[idx];
        map->nds[idx] = new;

        map->mapSize++;
        if((float)map->mapSize/map->listSize >= map->loadFactor)
                rehash(map);
        return ret;
}

void * getKey(HashMap * map, const void * key){
        Node * id = map->nds[map->hash(key) % map->listSize];
        while(id != NULL){
                if(map->eq(key, id-> key))
                        return id->value;
                id = id->next;
        }
        return NULL;
}


int hsInt(const void * v){return * (int *) v;}

int eqInt(const void * l, const void * r){return * (int *) l == * (int *) r; }

/*
int main(int argc, char ** argv){
        int i, * k, * v;
        HashMap * h = createMap(10, 1.0f, &hsInt, &eqInt);
        printf("Created map...\n");
        for(i = 1; i <= 10; i++){
                k = (int *) malloc(sizeof(int));
                v = (int *) malloc(sizeof(int));
                *k = i;
                *v = i*i;
                insert(h, k, v);
                printf("Inserted %d,%d\n", *k, *v);
        }

        removeKey(h, k);

        for(i = 1; i <= 10; i++){
                const int * v = (int *) getKey(h, &i);
                if(v != NULL)
                        printf("%d\n", *v);
                else
                        printf("NULL\n");
        }

        freeMap(h);
}
*/
