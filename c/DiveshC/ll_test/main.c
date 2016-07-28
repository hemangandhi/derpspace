#include <stdio.h>
#include <stdlib.h>
#include "linked_list.c"

void doNothingDealloc(void * IDontGiveAFuck){}

int main(int argc, char ** argv){
        Node * curr = NULL;
        for(int i = 0; i < argc; i++)
                curr = newNode(argv[i], curr); //*(argv + i)

        for(Node * temp = curr; temp != NULL; temp = temp->next)
                printf("%s\n", (char *) temp->val);

        delNode(curr, &doNothingDealloc);
        return 0;
}
