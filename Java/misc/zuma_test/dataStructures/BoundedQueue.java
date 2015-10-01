package dataStructures;

/**
 * Queue used by linked list.
 * Only a model to demo. implementation.
 * @author Heman
 *
 * @param <T>
 */
public class BoundedQueue<T> {

	private Object[] mem;
	private int rems, adds;
	
	public BoundedQueue(int cap){
		mem = new Object[cap];
	}
	
	public void enqueue(T item){
		if(adds - rems >= mem.length)
			throw new IllegalStateException("Queue is full!");
		
		mem[adds % mem.length] = item;
		adds++;
	}
	
	public T dequeue(){
		if(rems >= adds)
			throw new IllegalStateException("Queue is empty!");
		
		T t = (T) mem[rems % mem.length];
		rems++;
		return t;
	}
	
	public T[] vals(){
		Object[] temp = new Object[adds - rems];
		for(int i = 0; i < temp.length; i++)
			temp[i] = mem[(rems + i) % mem.length];
		return (T[]) temp;
	}
	
	public int size(){
		return adds - rems;
	}
}
