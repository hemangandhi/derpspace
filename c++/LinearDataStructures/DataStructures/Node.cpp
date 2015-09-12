#include "stdafx.h"
#include "Node.h"

template<typename T>
Node<T>::Node(T data, Node<T>* next) {
	this->data = data;
	this->next = next;
}


template<typename T>
Node<T>::~Node() {
//	delete &data;
	delete next;
}

template<typename T>
bool Node<T>::operator==(Node<T>& other) {
	return other.data == data && other.next == next;
}

template<typename T>
bool Node<T>::operator!=(Node<T>& other) {
	return !(*this == other);
}