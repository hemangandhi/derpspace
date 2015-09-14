#include "stdafx.h"
#include <stdexcept>
#include "SimpleQueue.h"

template<typename T>
void SimpleQueue<T>::push(T& data) {
	SimpleLinkedList<T>::addToTail(data);
}

template<typename T>
T& SimpleQueue<T>::peek(){
	return SimpleLinkedList<T>::operator[](0);
}

template<typename T>
T SimpleQueue<T>::pop() {
	return SimpleLinkedList<T>::removeHead();
}

template<typename T>
int SimpleQueue<T>::size() {
	return SimpleLinkedList<T>::size();
}