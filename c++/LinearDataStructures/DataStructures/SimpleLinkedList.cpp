#include "stdafx.h"
#include <stdexcept>
#include "SimpleLinkedList.h"

template<typename T>
SimpleLinkedList<T>::SimpleLinkedList() {
	head = nullptr;
	tail = nullptr;
	len = 0;
}

template<typename T>
SimpleLinkedList<T>::~SimpleLinkedList() {
	delete head;
}

template<typename T>
int SimpleLinkedList<T>::size() {
	return len;
}

template<typename T>
void SimpleLinkedList<T>::addToHead(T& data) {
	head = new Node<T>(data, head);
	len++;
	if (len == 1)
		tail = head;
}

template<typename T>
void SimpleLinkedList<T>::addToTail(T& data) {
	if (len == 0) {
		addToHead(data);
	} else {
		tail->next = new Node<T>(data, nullptr);
		tail = tail->next;
		len++;
	}
}

template<typename T>
void SimpleLinkedList<T>::addAt(int ind, T& data) {
	if (ind == 0)
		addToHead(data);
	else if (ind == len)
		addToTail(data);
	else if (ind < 0 || ind > len)
		throw std::out_of_range("Index out of bounds.");
	else {
		Node<T>* prev = head;
		Node<T>* nxt = head->next;
		while (ind > 1) {
			prev = nxt;
			nxt = nxt->next;
			ind--;
		}
		prev->next = new Node<T>(data, nxt);
	}
}

template<typename T>
int SimpleLinkedList<T>::indexOf(T& data) {
	int i = 0;
	for (Node<T>* temp = head; temp != nullptr; temp = temp->next) {
		if (temp->data == data)
			return i;
		i++;
	}
	return -1;
}

template<typename T>
bool SimpleLinkedList<T>::contains(T& data) {
	return indexOf(data) > -1;
}

template<typename T>
T SimpleLinkedList<T>::remove(int ind) {
	if (ind < 0 || ind >= len)
		throw std::out_of_range("Index out of bounds.");

	Node<T>* prev = nullptr, * toRem = nullptr;
	toRem = head;
	while (ind > 0) {
		prev = toRem;
		toRem = toRem->next;
		ind--;
	}

	if (prev != nullptr)
		prev->next = toRem->next;
	else
		head = toRem->next;

	toRem->next = nullptr;
	T toRet = *(new T(toRem->data));
	delete toRem;
	return toRet;
}

template<typename T>
T SimpleLinkedList<T>::removeHead() {
	return remove(0);
}

template<typename T>
T& SimpleLinkedList<T>::operator[](int ind) {
	if (ind < 0 || ind >= len)
		throw std::out_of_range("Index out of bounds.");

	Node<T>* temp = nullptr;
	for (temp = head; temp != nullptr && ind > 0; temp = temp->next)
		ind--;
	return temp->data;
}

template<typename T>
bool SimpleLinkedList<T>::operator==(SimpleLinkedList<T>& other) {
	return head == other.head;
}

template<typename T>
my_iterator<T> SimpleLinkedList<T>::begin() {
	return my_iterator<T>(head);
}

template<typename T>
my_iterator<T> SimpleLinkedList<T>::end() {
	return my_iterator<T>();
}