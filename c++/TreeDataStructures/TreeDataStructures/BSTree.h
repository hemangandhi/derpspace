#pragma once
#include "BinaryNode.h"

template<typename T>
class BSTree {
public:
	BSTree() : root(nullptr) {}
	void add(const T& data);
	bool search(const T& data) const;
	BSTree& operator+= (const T& data);
private:
	BinaryNode<T>* root;
};

template<typename T>
void BSTree::add(const T& data) {
	
}