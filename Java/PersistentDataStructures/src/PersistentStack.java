
public class PersistentStack<T> {

	private Node<T> top;
	private int size;
	
	public PersistentStack(){
		this(null, 0);
	}
	
	public PersistentStack(Node<T> top){
		this(top,0);
	}
	
	public PersistentStack(Node<T> top, int size){
		this.top = top;
		this.size = size;
	}
	
	public PersistentStack<T> push(T add){
		return new PersistentStack<T>(new Node<T>(add,top), size + 1);
	}
	
	public T peek(){
		if(top == null)
			return null;
		return top.data();
	}
	
	public PersistentStack<T> pop(){
		if(top == null)
			throw new IllegalStateException("Stack is empty!");
		return new PersistentStack<T>(top.next(), size - 1);
	}
	
	public int size(){
		return size;
	}
}
