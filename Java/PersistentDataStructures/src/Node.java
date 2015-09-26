
public class Node<T> {

	private T data;
	private Node<T> next;
	
	public Node(T data, Node<T> next){
		this.data = data;
		this.next = next;
	}
	
	public Node<T> next(){
		return next;
	}
	
	public T data(){
		return data;
	}
}
