#pragma once
#include "SimpleLinkedList.h"
#include "Node.h"

template<typename T>
class SimpleStack: private SimpleLinkedList<T> {
public:
	//SimpleStack();
	//~SimpleStack();
	void push(T& data);
	T pop();
	T& peek();
	int size();
};