#include "stdafx.h"
#include "PersistentStack.h"

template<typename T>
PersistentStack<T>::PersistentStack() {
	head = nullptr;
	lower = nullptr;
	upper = 0;
}

template<typename T>
PersistentStack<T>::PersistentStack(Node<T>& head) {
	this->head = &head;
	lower = nullptr;
	upper = 0;
}

template<typename T> //PRIVATE
PersistentStack<T>::PersistentStack(Node<T>& head, PersistentStack* lower, int upper) {
	this->head = &head;
	this->lower = lower;
	this->upper = upper;
}

template<typename T>
PersistentStack<T>::~PersistentStack() {
	if (upper == 0) {
		if (this->isBottom()) {
			delete head;
			return;
		}
		lower->upper--;
		Node<T>* t = head;
		while (t->next != lower->head && t->next != nullptr)
			t = t->next;
		t->next = nullptr;
		if (lower->upper == 0)
			delete lower;
		delete head;
	}
}

template<typename T>
PersistentStack<T>* PersistentStack<T>::push(T data) {
	Node<T> toAdd = *(new Node<T>(data, head));
	upper++;
	return (new PersistentStack<T>(toAdd, this, 0));
}

template<typename T>
PersistentStack<T>* PersistentStack<T>::pop() {
	if (this->isBottom())
		throw - 1;
	PersistentStack* l = lower;
	lower = new PersistentStack<T>(*(head->next), l, 1);
	return lower;
}

template<typename T>
T& PersistentStack<T>::peek() {
	if (head == nullptr)
		throw - 1;
	return head->data;
}

template<typename T>
bool PersistentStack<T>::isBottom() {
	return lower == nullptr;
}