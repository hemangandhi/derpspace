#include <stdio.h>

int div(int x){return x;}

int main(int argc, char** argv){//0 or 2. argc and argv are conventionally used.
        //argc = # of cmd ln args.
        //argv =
        int i;
        for(i = 0; i < argc; i++){
                printf("Arg[%d] is %s \n", i, argv[i]);
        }
        return i;
}

