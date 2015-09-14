#pragma once
#include <iterator>
#include "Node.h"

template<typename T>
class my_iterator : public std::iterator<std::forward_iterator_tag, T> {
public:
	my_iterator() { cursor = nullptr; }
	my_iterator(Node<T>* temp) { cursor = temp; }
	void swap(my_iterator<T>& other) {
		std::swap(cursor, other.cursor);
	}
	my_iterator<T>& operator++() {
		if (cursor == nullptr)
			throw std::out_of_range("Iterator out of bounds.");
		cursor = cursor->next;
		return *this;
	}
	my_iterator<T>& operator++(int j) {
		if (cursor == nullptr)
			throw std::out_of_range("Iterator out of bounds.");
		my_iterator<T>& tmp = *this;
		cursor = cursor->next;
		return tmp;
	}
	bool operator==(my_iterator<T>& other) {
		return other.cursor == cursor;
	}
	bool operator!=(my_iterator<T>& other) {
		return !(cursor == other.cursor);
	}
	T& operator*() {
		if (cursor == nullptr) {
			throw std::out_of_range("Iterator out of bounds.");
		}
		return cursor->data;
	}
	T& operator->() {
		return this->operator*();
	}
private:
	Node<T>* cursor;
};

template<typename T>
class SimpleLinkedList{
public:
	SimpleLinkedList();
	virtual ~SimpleLinkedList();
	int size();
	void addToHead(T& data);
	void addToTail(T& data);
	void addAt(int ind, T& data);
	int indexOf(T& data);
	bool contains(T& elem);
	T removeHead();
	T remove(int index);
	T& operator[](int index);
	bool operator==(SimpleLinkedList& other);
	my_iterator<T> begin();
	my_iterator<T> end();
private:
	Node<T>* head;
	Node<T>* tail;
	int len;
};
