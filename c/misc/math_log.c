#include <math.h>
#include <stdio.h>

int main(){
    int i;
    for(i = 0; i < 64; i++){
        printf("log(%d) = %d\n", i, (int) log2(i));
    }
}
