#Data Structures in Java

This project has all the data structures and applications made in a college
Data Structures class (namely, CS 112 of Rutgers).

The linked lists package has a regular singly linked list, a circular variant, a
doubly linked version and a circular doubly linked version. The stack is a special 
regular linked list and the queue is a special circular one. Merge sort is provided
for the regular linked list.

The trees package has a binary tree, a binary search tree, an AVL tree and a Max Heap.
Heap sort is implemented in the heap class and the AVL Tree class has tree sort. The
updatable heap uses the hash map from hashing.

The hashing package contains a hash map with an amortized rehashing algorithm.

The graphs package has a simple weighted, directed graph that uses DFS to see path existence, 
a topological sort and also uses the updatable heap from trees (and the regular linked list) 
to perform Dijkstra's shortest path algorithm. The adjacency linked list is replaced with an
adjacency hash map for efficiency and this hash map is the one implemented in hashing.

The apps package has a Huffman coder and implementations of quick sort and radix sort.
The Huffman coder uses the heap and hash maps implemented in other packages. The radix
sort uses the regular linked list.