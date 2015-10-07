#pragma once
#include <iostream>
#include "BinaryNode.h"

using namespace std;

template<typename T>
class BSTree {
public:
	friend ostream& operator<<(ostream& out, const BSTree<T>& arg);
	BSTree() : root(nullptr) {}
	~BSTree();
	void add(const T& data);
	bool search(const T& data) const;
	BSTree<T>& operator+= (const T& data);
private:
	BinaryNode<T>* root;
};

template<typename T>
BSTree<T>::~BSTree() {
	delete root;
}

template<typename T>
void BSTree<T>::add(const T& data) {
	if (root == nullptr) {
		root = new BinaryNode<T>(data, nullptr, nullptr);
		return;
	}
	for (BinaryNode<T>* tmp = root; tmp != nullptr;) {
		if (data > tmp->data) {
			if (tmp->right == nullptr) {
				tmp->right = new BinaryNode<T>(data, nullptr, nullptr);
				return;
			}else {
				tmp = tmp->right;
			}
		}else {
			if (tmp->left == nullptr) {
				tmp->left = new BinaryNode<T>(data, nullptr, nullptr);
				return;
			}else {
				tmp = tmp->left;
			}
		}
	}
}

template<typename T>
BSTree<T>& BSTree<T>::operator+= (const T& data) {
	add(data);
	return *this;
}

template<typename T>
bool BSTree<T>::search(const T& data) const {
	for (BinaryNode<T>* tmp = root; tmp != nullptr;) {
		if (data == tmp->data)
			return true;
		else if (data > tmp->data)
			tmp = tmp->right;
		else
			tmp = tmp->left;
	}

	return false;
}

template<typename T>
ostream& operator<<(ostream& out, const BSTree<T>& arg) {
	if (arg.root == nullptr)
		return out << "Empty tree!" << endl;
	return out << *arg.root;
}

ostream& operator<<(ostream& out, const BSTree<int>& arg) {
	if (arg.root == nullptr) {
		out << "Empty tree!" << endl;
		return out;
	}
	out << *arg.root;
	return out;
}

template class BSTree<int>;