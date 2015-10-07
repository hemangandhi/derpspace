#pragma once

template<typename T>
class BinaryNode {
public:
	BinaryNode<T> left, right;
	T data;
	BinaryNode(T data, BinaryNode<T> l, BinaryNode<T> r) {
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

template class BinaryNode<int>;