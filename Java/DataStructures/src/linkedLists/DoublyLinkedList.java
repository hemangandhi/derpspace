package linkedLists;

public class DoublyLinkedList<T> {

	private static class Node<U>{
		public U data;
		public Node<U> next, prev;
		
		public Node(U d, Node<U> n, Node<U> p){
			data = d;
			next = n;
			prev = p;
		}
	}
	
	private Node<T> head, tail;
	private int size;
	
	public void addToHead(T val){
		if(size == 0){
			head = new Node<T>(val, null, null);
			tail = head;
		}else{
			head = new Node<T>(val, head, null);
			head.next.prev = head;
		}
		
		size++;
	}
	
	public void addToTail(T val){
		if(size == 0)
			addToHead(val);
		else{
			tail.next = new Node<T>(val, null, tail);
			tail = tail.next;
			size++;
		}
	}
	
	public void add(int ind, T val){
		if(ind < 0 || ind > size)
			throw new IllegalArgumentException("Index out of bounds!");
		else if(ind == 0)
			addToHead(val);
		else if(ind == size)
			addToTail(val);
		else{
			Node<T> t;
			for(t = head; ind > 1; ind--, t = t.next);
			
			t.next = new Node<T>(val, t.next, t);
			size++;
		}
	}
	
	public T removeHead(){
		if(size == 0)
			throw new IllegalStateException("Can't remove from empty list!");
		else{
			T r = head.data;
			if(size == 1)
				head = tail = null;
			else{
				head = head.next;
				head.prev = null;
			}
			size--;
			return r;
		}
	}
	
	public T removeTail(){
		if(size == 0)
			throw new IllegalStateException("Can't remove from empty list!");
		else{
			T r = tail.data;
			if(size == 1)
				head = tail = null;
			else{
				tail = tail.prev;
				tail.next = null;
			}
			size--;
			return r;
		}
	}
	
	public T remove(int ind){
		if(size == 0)
			throw new IllegalStateException("Can't remove from empty list!");
		else if(ind < 0 || ind >= size)
			throw new IllegalArgumentException("Index out of bounds!");
		else if(ind == 0)
			return removeHead();
		else if(ind == size - 1)
			return removeTail();
		else{
			Node<T> t = head;
			for(;ind > 1; ind--, t = t.next);
			
			T r = t.next.data;
			t.next = t.next.next;
			t.next.prev = t;
			size--;
			return r;
		}
	}
}
