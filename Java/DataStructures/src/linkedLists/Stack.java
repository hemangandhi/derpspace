package linkedLists;

public class Stack<T> {

	private static class Node<U>{
		public U data;
		public Node<U> next;
		
		public Node(U d, Node<U> n){
			data = d;
			next = n;
		}
	}
	
	private Node<T> head;
	private int size;
	
	public void push(T d){
		if(size == 0)
			head = new Node<T>(d, null);
		else
			head = new Node<T>(d, head);
		
		size++;
	}
	
	public T pop(){
		if(size == 0)
			throw new IllegalArgumentException("Can't pop off empty stack!");
		else{
			T r = head.data;
			head = head.next;
			size--;
			return r;
		}
	}
	
	public T peek(){
		if(size == 0)
			return null;
		else
			return head.data;
	}
	
	public int size(){
		return size;
	}
}
