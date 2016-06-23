#include <stdio.h>
int main(int argc, char ** argv){
        char v, * ptr;
        ptr = &v;
        scanf("%c", ptr);
        while(*ptr != 0){
        ptr+= 1;
        (*ptr) += 1;
        printf("%c", *ptr);
        ptr-= 1;
        (*ptr) -= 1;

        }
        return 0;
}
