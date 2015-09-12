#pragma once
#include "Node.h"

template<typename T>
class PersistentStack {
public:
	PersistentStack();
	PersistentStack(Node<T>& head);
	~PersistentStack();
	PersistentStack<T>* push(T data);
	PersistentStack<T>* pop();
	T& peek();
	bool isBottom();
private:
	PersistentStack(Node<T>& head, PersistentStack* lower, int upper);
	Node<T>* head;
	PersistentStack* lower;
	int upper;

};
