#include <stdio.h>
int * returnShit(){
        int i = 0;
        return &i;
}

int * other(){
        int j = 1;
        return NULL;
}

int main(){
        int * v = returnShit();
        other();
        printf("%d\n", *v);
}
