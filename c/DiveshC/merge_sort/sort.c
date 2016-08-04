#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* A generic swap of two pointers pointing to size bytes. */
void swap(void * src, void * dest, int size){
        //malloc a temp
        void * temp = malloc(size);
        //temp = src
        memcpy(temp, src, size);
        //src = dest
        memcpy(src, dest, size);
        //dest = temp
        memcpy(dest, temp, size);
        //no garbage.
        free(temp);
}

/*Much better than java.lang.Comparator<T>*/
typedef int (* Comparer)(const void *, const void *);


void merge(const void * src, void * dest, int start, int mid, int end, int size, Comparer comp){
        int l, r, o;//left, right, output.
        for(l = start, r = mid, o = start; l < mid && r < end; o++){
                //Compare values.
                int cmp = comp(src + size * l, src + size * r);
                if(cmp == 0){
                        //If equal, advance both ptrs and add two values to the output.
                        memcpy(dest + o * size, src + l * size, size);
                        o++; l++;
                        memcpy(dest + o * size, src + r * size, size);
                        r++;
                }else if(cmp < 0){
                        //If the left was less, add it.
                        memcpy(dest + o * size, src + l * size, size);
                        l++;
                }else{
                        //If the right was less, add it.
                        memcpy(dest + o * size, src + r * size, size);
                        r++; 
                }
        }

        if(l < mid){
                //Add left overs.
                memcpy(dest + size * o, src + size * l, (mid - l) * size);
        }else if(r < end){
                //Add "right" overs.
                memcpy(dest + size * o, src + size * r, (end - l) * size);
        }
}

static void sortLocal(void * src, void * dest, int start, int end, int size, Comparer comp){
        if(start < end - 1){
                int mid = (start + end) / 2;
                sortLocal(src, dest, start, mid, size, comp);
                sortLocal(src, dest, mid, end, size, comp);
                merge(src, dest, start, mid, end, size, comp);
                memcpy(src + start * size, dest + start * size, (end - start) * size);
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
