#include <stdio.h>

int main () {
    int vals[2][2]= {{1, 2}, {3, 4}};
    int * v = *vals + 1;
    int * w = *(vals + 1);
    printf("*v = %d and *w = %d\n", *v, *w);
}
