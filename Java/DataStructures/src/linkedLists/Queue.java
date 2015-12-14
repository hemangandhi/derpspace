package linkedLists;

public class Queue<T> {

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
	
	public void enqueue(T val){
		if(size == 0){
			rear = new Node<T>(val, null);
			rear.next = rear;
		}else{
			rear.next = new Node<T>(val, rear.next);
			rear = rear.next;
		}
		
		size++;
	}
	
	public T dequeue(){
		if(size == 0)
			throw new IllegalStateException("Cannot dequeue off empty queue!");
		else{
			T r = rear.next.data;
			rear.next = rear.next.next;
			size--;
			return r;
		}
	}
	
	public T peek(){
		if(size == 0)
			return null;
		else
			return rear.next.data;
	}
	
	public int size(){
		return size;
	}
}
