#include "stdafx.h"
#include "Node.cpp"
#include "SimpleStack.cpp"

template class Node<int>;
template class SimpleStack<int>;

void testSimpleStack() {
	printf("TESTING: Simple stack!");
	SimpleStack<int>* st = new SimpleStack<int>();
	try {
		st->peek();
	}catch (std::out_of_range& orr) {
		printf("Error works!\n");
	}
	int add = 8;
	st->push(add);
	printf("%d on top.\n", st->peek());
	delete st;
	printf("done");
	getchar();
}


int main() {
	testSimpleStack();
	return 0;
}

