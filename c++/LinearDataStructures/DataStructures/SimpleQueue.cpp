#include "stdafx.h"
#include <stdexcept>
#include "SimpleQueue.h"

template<typename T>
SimpleQueue<T>::SimpleQueue() {
	head = nullptr;
	tail = nullptr;
	len = 0;
}

template<typename T>
SimpleQueue<T>::~SimpleQueue() {
	delete head;
}

template<typename T>
void SimpleQueue<T>::push(T& data) {
	if (head == nullptr) {
		head = new Node<T>(data, tail);
		tail = head;
	}else {
		tail->next = new Node<T>(data, nullptr);
		tail = tail->next;
	}
	len++;
}

template<typename T>
T& SimpleQueue<T>::peek(){
	if (len == 0)
		throw std::out_of_range("Queue is empty");
	return head->data;
}

template<typename T>
T& SimpleQueue<T>::pop() {
	if (len == 0)
		throw std::out_of_range("Queue is empty");
	len--;
	Node<T>* old = head;
	head = head->next;
	T& toR = old->data;
	old->next = nullptr;
	delete old;
	return toR;
}

template<typename T>
int SimpleQueue<T>::size() {
	return len;
}