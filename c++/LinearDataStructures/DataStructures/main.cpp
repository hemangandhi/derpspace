#include "stdafx.h"
#include "Node.cpp"
#include "SimpleStack.cpp"
#include "SimpleQueue.cpp"

template class Node<int>;
template class SimpleStack<int>;
template class SimpleQueue<int>;

void testSimpleStack() {
	printf("TESTING: Simple stack!\n");
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

void testSimpleQueue() {
	printf("TESTING: Simple queue!\n");
	SimpleQueue<int>* st = new SimpleQueue<int>();
	try {
		st->peek();
	}
	catch (std::out_of_range& orr) {
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
//	testSimpleStack();
	testSimpleQueue();
	return 0;
}

