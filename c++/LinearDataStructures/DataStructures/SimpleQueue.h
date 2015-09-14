#pragma once
#include "Node.h"
#include "SimpleLinkedList.h"

template<typename T>
class SimpleQueue: private SimpleLinkedList<T> {
public:
	//SimpleQueue();
	//~SimpleQueue();
	void push(T& data);
	T& peek();
	T pop();
	int size();
};