#pragma once
#include "Node.h"

template<typename T>
class SimpleStack {
public:
	SimpleStack();
	~SimpleStack();
	void push(T& data);
	T& pop();
	T& peek();
	int size();
private:
	Node<T>* head;
	int len;

};