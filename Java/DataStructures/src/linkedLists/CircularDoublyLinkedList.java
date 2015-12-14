package linkedLists;

public class CircularDoublyLinkedList<T> {

	private static class Node<U>{
		public U data;
		public Node<U> next, prev;
		
		public Node(U d, Node<U> n, Node<U> p){
			data = d;
			next = n;
			prev = p;
		}
	}
	
	private Node<T> rear;
	private int size;
	
	public void addToHead(T val){
		if(size == 0){
			rear = new Node<T>(val, null, null);
			rear.next = rear;
			rear.prev = rear;
		}else{
			rear.next = new Node<T>(val, rear.next, rear);
			rear.next.next.prev = rear.next;
		}
		
		size++;
	}
	
	public void addToRear(T val){
		addToHead(val);
		rear = rear.next;
	}
	
	public void add(int ind, T val){
		if(ind < 0 || ind > size)
			throw new IllegalArgumentException("Index out of bounds!");
		else if(ind == 0)
			addToHead(val);
		else if(ind == size)
			addToRear(val);
		else{
			Node<T> t;
			for(t = rear; ind > 0; t = t.next, ind--);
			t.next = new Node<T>(val, t.next, t);
			t.next.next.prev = t.next;
		}
	}
	
	public T removeHead(){
		if(size == 0)
			throw new IllegalStateException("Can't remove from empty list!");
		else{
			T r = rear.next.data;
			rear.next = rear.next.next;
			rear.next.prev = rear;
			size--;
			if(size == 0)
				rear = null;
			return r;
		}
	}
	
	public T removeTail(){
		rear = rear.prev;
		return removeHead();
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
			Node<T> t;
			for(t = rear; ind > 0; ind--, t = t.next);
			
			T r = t.next.data;
			t.next = t.next.next;
			t.next.prev = t;
			return r;
		}
	}
}
