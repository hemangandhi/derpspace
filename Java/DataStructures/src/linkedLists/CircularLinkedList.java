package linkedLists;

public class CircularLinkedList<T> {

	private static class Node<U>{
		public U data;
		public Node<U> next;
		
		public Node(U d, Node<U> n){
			data = d;
			next = n;
		}
	}
	
	private Node<T> rear;
	private int size;
	
	public void addToHead(T val){
		if(rear == null){
			rear = new Node<T>(val, null);
			rear.next = rear;
		}else{
			rear.next = new Node<T>(val, rear.next);
		}
		
		size++;		
	}
	
	public void addToTail(T val){
		addToHead(val);
		rear = rear.next;
	}
	
	public void addAt(int ind, T val){
		if(ind < 0 || ind > size)
			throw new IllegalArgumentException("Index out of bounds!");
		else if(ind == 0)
			addToHead(val);
		else if(ind == size)
			addToTail(val);
		else{
			Node<T> t;
			for(t = rear; ind > 0; t = t.next, ind--);
			t.next = new Node<T>(val, t.next);
			
			size++;
		}
	}
	
	public T removeHead(){
		if(size == 0)
			throw new IllegalStateException("Can't remove from empty list!");
		else{
			T r = rear.next.data;
			rear = rear.next.next;
			size--;
			if(size == 0)
				rear = null;
			return r;
		}
	}
	
	public T remove(int ind){
		if(size == 0)
			throw new IllegalStateException("Can't remove from empty list!");
		else if(ind < 0 || ind >= size)
			throw new IllegalArgumentException("Index out of bounds!");
		else if(ind == 0 || size == 1)
			return removeHead();
		else{
			Node<T> t;
			for(t = rear; ind > 0; t = t.next, ind--);
			
			if(t.next == rear)
				rear = t;
			
			T r = t.data;
			t = t.next.next;
			size--;
			return r;
		}
	}
	
	public int size(){
		return size;
	}
}
