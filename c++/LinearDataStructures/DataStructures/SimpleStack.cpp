#include "stdafx.h"
#include <stdexcept>
#include "SimpleStack.h"

using namespace std;
template<typename T>
void SimpleStack<T>::push(T& data) {
	SimpleLinkedList<T>::addToHead(data);
}

template<typename T>
T& SimpleStack<T>::peek() {
	return SimpleLinkedList<T>::operator[](0);
}

template<typename T>
T SimpleStack<T>::pop() {
	return SimpleLinkedList<T>::removeHead();
}

template<typename T>
int SimpleStack<T>::size() {
	return SimpleLinkedList<T>::size();
}