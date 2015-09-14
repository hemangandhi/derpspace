#include "stdafx.h"
#include "Node.cpp"
#include "SimpleLinkedList.cpp"
#include "SimpleStack.cpp"
#include "SimpleQueue.cpp"

template class Node<int>;
template class SimpleLinkedList<int>;
template class my_iterator<int>;
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
	int add2 = 9;
	st->push(add2);
	printf("%d on top.\n", st->pop());
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

void testLinkedList() {
	printf("Testing linked list!\n");
	SimpleLinkedList<int>* ll = new SimpleLinkedList<int>();
	for (int i = 0; i < 10; i++)
		ll->addToTail(i);
	ll->addAt(6, *(new int(8)));
	ll->remove(6);
	for (my_iterator<int> itr = ll->begin(); itr != ll->end(); itr++)
		printf("%d\n",*itr);
	printf("Index of 9: %d \n", ll->indexOf(*(new int(9))));
	getchar();
}

int main() {
//	testSimpleStack();
//	testSimpleQueue();
	testLinkedList();
	return 0;
}

