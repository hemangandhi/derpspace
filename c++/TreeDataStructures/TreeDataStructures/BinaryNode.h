#pragma once
#include <iostream>

using namespace std;

template<typename T>
class BinaryNode {
public:
	BinaryNode<T>* left, * right;
	T data;
	friend ostream& operator<<(ostream& out, BinaryNode<T> node);
	BinaryNode(T data, BinaryNode<T>* l, BinaryNode<T>* r) {
		this->data = data;
		left = l;
		right = r;
	}
	~BinaryNode() {
		delete left;
		delete right;
		delete &data;
	}
};

template<typename T>
ostream& operator<<(ostream& out, BinaryNode<T> node) {
	out << "Data :" << node.data << endl;
	if (node.left == nullptr)
		out << "This node has no left child." << endl;
	else
		out << "Left child:" << *node.left;

	if (node.right == nullptr)
		out << "This node has no right child." << endl;
	else
		out << "Right child: " << *node.right;

	out << "That's all!" << endl;

	return out;
}


ostream& operator<<(ostream& out, BinaryNode<int> node) {
	out << "Data :" << node.data << endl;
	out << "Left child:" << endl;
	if (node.left == nullptr) {
		out << "None" << endl;
	}else {
		out << *node.left << endl;
	}

	out << "Right Child:" << endl;
	if (node.right == nullptr) {
		out << "None" << endl;
	}else {
		out << *node.right << endl;
	}

	out << "That's all!" << endl;

	return out;
}

template class BinaryNode<int>;