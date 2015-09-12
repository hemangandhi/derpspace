#pragma once
#include "Node.h"

template<typename T>
class SimpleQueue {
public:
	SimpleQueue();
	~SimpleQueue();
	void push(T& data);
	T& peek();
	T& pop();
	int size();
private:
	Node<T>* head;
	Node<T>* tail;
	int len;
};