#include <stdlib.h>
#include <stdio.h>

int main(){
    char ** b = (char **) malloc(sizeof(char *) * 10);
    *b = "hello world";
    b[0][1];
}
