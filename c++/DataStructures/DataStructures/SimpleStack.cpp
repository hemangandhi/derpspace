#include "stdafx.h"
#include <stdexcept>
#include "SimpleStack.h"

using namespace std;

template<typename T>
SimpleStack<T>::SimpleStack() {
	head = nullptr;
	len = 0;
}

template<typename T>
SimpleStack<T>::~SimpleStack() {
	delete head;
}

template<typename T>
void SimpleStack<T>::push(T& data) {
	len++;
	head = new Node<T>(data, head);
}

template<typename T>
T& SimpleStack<T>::peek() {
	if (len == 0) {
		throw out_of_range("Stack is empty");
	}
	return head->data;
}

template<typename T>
T& SimpleStack<T>::pop() {
	if (len == 0)
		throw out_of_range("Stack is empty");
	len--;
	T& toRet = head->data;
	head = head->next;
	return toRet;
}

template<typename T>
int SimpleStack<T>::size() {
	return len;
}