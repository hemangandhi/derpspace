#pragma once

template<typename T>
class Node{
public:
	Node(T data, Node* next);
	~Node();
	bool operator==(Node<T>& other);
	bool operator!=(Node<T>& other);
	T data;
	Node* next;
};