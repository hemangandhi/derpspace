#pragma once
#include "Node.h"

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
private:
	Node<T>* head;
	Node<T>* tail;
	int len;
};
