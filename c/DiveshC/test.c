#include <stdio.h>

int main(int argc, char ** argv){
        int a[5];
        for(int i = 0, ctr = 0; i <= 7; i++, ctr++){
                a[5 - i] = 0;
                printf("%d\n", ctr);
        }
        return 0;
}
