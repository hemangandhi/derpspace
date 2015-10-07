// TreeDataStructures.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "BSTree.h"

using namespace std;

int main(){
	BSTree<int>* bs = new BSTree<int>();
	cout << *bs;
	bs->add(9);
//	cout << *bs;
	bs->add(8);
//	cout << *bs;
	bs->add(10);
//	cout << *bs;
	*bs += 11;
	cout << boolalpha;
	cout << bs->search(11) << endl;
	cout << *bs;
	getchar();
}

