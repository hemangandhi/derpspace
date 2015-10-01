package dataStructures;

/**
 * A linked list to store targets.
 * 
 * Just a sample of the implementation.
 * @author Heman
 *
 * @param <T>
 */
public class LinkedList<T extends Comparable<T>> {

	private Node<T> head;
	private int size;
	
	public void addToHead(T item){
		head = new Node<T>(item, head, null);
		if(head.next != null)
			head.next.prev = head;
		size++;
	}
	
	public void add(int index, T data){
		if(index < 0 || index > size)
			throw new IllegalArgumentException("Index out of bounds!");
		
		if(index == 0){
			addToHead(data);
			return;
		}	
		
		Node<T> temp = head;
		for(int i = 1; i < index; i++)
			temp = temp.next;
		
		temp.next = new Node<T>(data, temp.next, temp);
		if(temp.next.next != null)
			temp.next.next.prev = temp.next.next;
		
		size++;
	}
	
	public int removeMatchingAdj(int index){
		if(index < 0 || index >= size)
			throw new IllegalArgumentException("Index out of bounds!");
		
		Node<T> temp = head;
		for(int i = 0; i < index; i++)
			temp = temp.next;
		
		Node<T> left = temp, right = temp;
		int rems = 0, b;
		do{
			b = rems;
			if(left.prev != null && left.prev.data.compareTo(temp.data) == 0){
				left = left.prev;
				rems++;
			}	
			if(right.next != null && right.next.data.compareTo(temp.data) == 0){
				right = right.next;
				rems++;
			}	
		}while(b != rems);
		
		if(rems == 0)
			return 0;
		
		if(left == head)
			head = right.next;
		
		if(right.next != null)
			right.next.prev = left.prev;
		if(left.prev != null)
			left.prev.next = right.next;
		
		rems++;
		size -= rems;
		
		return rems;
	}
	
	public int size(){
		return size;
	}
	
	public Object[] vals(){
		Object[] vals = new Object[size];
		int i = 0;
		for(Node<T> t = head; t != null; t = t.next){
			vals[i] = t.data;
			i++;
		}	
		
		return vals;
	}
	
	public String toString(){
		String r = "";
		for(Node<T> t = head; t != null; t = t.next)
			r += "->" + t.data;
		return r;
	}
}
