#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void swap(void * src, void * dest, int size){
        void * temp = malloc(size);
        memcpy(temp, src, size);
        memcpy(src, dest, size);
        memcpy(dest, temp, size);
        free(temp);
}

typedef int (* Comparer)(const void *, const void *);

void merge(const void * src, void * dest, int start, int mid, int end, int size, Comparer comp){
        int l, r, o;
        for(l = start, r = mid, o = start; l < mid && r < end; o++){
                int cmp = comp(src + size * l, src + size * r);
                if(cmp == 0){
                        memcpy(dest + o * size, src + l * size, size);
                        o++; l++;
                        memcpy(dest + o * size, src + r * size, size);
                        r++;
                }else if(cmp < 0){
                        memcpy(dest + o * size, src + l * size, size);
                        l++;
                }else{
                        memcpy(dest + o * size, src + r * size, size);
                        r++; 
                }
        }

        if(l < mid){
                memcpy(dest + size * o, src + size * l, (mid - l) * size);
        }else if(r < end){
                memcpy(dest + size * o, src + size * r, (end - l) * size);
        }
}

static void sortLocal(void * src, void * dest, int start, int end, int size, Comparer comp){
        if(start < end - 1){
                int mid = (start + end) / 2;
                sortLocal(src, dest, start, mid, size, comp);
                sortLocal(src, dest, mid, end, size, comp);
                merge(src, dest, start, mid, end, size, comp);
                memcpy(src + start * size, dest + start * size, (end - start) * size);
        }
}

void * sort(void * in, int elemCt, int size, Comparer comp){
        void * dest = malloc(elemCt * size);
        sortLocal(in, dest, 0, elemCt, size, comp);
        free(dest);
        return in;
}

static int myStrCmp(const void * l, const void * r){
        return strcmp(* (char **) l, * (char **) r);
}

int main(int argc, char ** argv){
        char ** sorted = sort(argv + 1, argc - 1, sizeof(char *), myStrCmp);
        for(int i = 0; i < argc - 1; i++)
                printf("%s\n", sorted[i]);
}
