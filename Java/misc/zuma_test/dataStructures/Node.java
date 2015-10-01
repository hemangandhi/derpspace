package dataStructures;

/**
 * Node used by linked list.
 * Model only for implementation details.
 * @author Heman
 *
 * @param <T>
 */
public class Node<T> {

	public T data;
	public Node<T> next;
	public Node<T> prev;
	
	public Node(T data, Node<T> next, Node<T> prev){
		this.data = data;
		this.next = next;
		this.prev = prev;
	}
}
