package linkedLists;

import java.util.ArrayList;

public class LinkedList<T> {

	private static class Node<U>{
		public U data;
		public Node<U> next;
		
		public Node(U d, Node<U> n){
			data = d;
			next = n;
		}
	}
	
	private Node<T> head, tail;
	private int size;
	
	/**
	 * Runtime: f(size) = size all the time, O(n)
	 * @return a copy of the linked list
	 */
	public LinkedList<T> copy(){
		LinkedList<T> r = new LinkedList<T>();
		
		for(Node<T> h = head; h != null; h = h.next)
			r.addToTail(h.data);
		
		return r;
	}
	
	/**
	 * Runtime: O(n) always.
	 * @return A copy of the linked list as an arraylist.
	 */
	public ArrayList<T> toArrayList(){
		ArrayList<T> r = new ArrayList<T>(size);
		for(Node<T> t = head; t != null; t = t.next)
			r.add(t.data);
		
		return r;
	}
	
	/**
	 * Runtime: O(1)
	 */
	public void clear(){
		head = tail = null;
		size = 0;
	}
	
	/**
	 * Adds other list to end of this one, clears other list.
	 * O(1)
	 * @param other
	 */
	public void join(LinkedList<T> other){
		if(other.size > 0){
			tail.next = other.head;
			tail = other.tail;
			size += other.size;
			
			other.head = other.tail = null;
			other.size = 0;
		}
	}
	
	/**
	 * O(1)
	 * @param val
	 */
	public void addToHead(T val){
		if(head == null){
			head = new Node<T>(val, null);
			tail = head;
		}else{
			head = new Node<T>(val, head);
		}
		
		size++;
	}
	
	/**
	 * O(1) thanks to the pointer to the tail.
	 * @param val
	 */
	public void addToTail(T val){
		if(head == null){
			addToHead(val);
		}else{
			tail.next = new Node<T>(val, null);
			tail = tail.next;
			size++;
		}
	}
	
	/**
	 * O(ind) - runs the for loop, updating a temp variable exactly ind times.
	 * @param ind
	 * @param val
	 */
	public void add(int ind, T val){
		if(ind < 0 || ind > size)
			throw new IllegalArgumentException("Index of of bounds");
		else if(ind == 0)
			addToHead(val);
		else if(ind == size)
			addToTail(val);
		else{
			Node<T> tmp;
			for(tmp = head; ind > 1; ind--, tmp = tmp.next);
			tmp.next = new Node<T>(val,tmp);
			size++;
		}
	}
	
	/**
	 * O(1) thanks to pointer to tail.
	 * @return
	 */
	public T getTail(){
		return tail.data;
	}
	
	/**
	 * O(1)
	 * @return
	 */
	public T removeHead(){
		if(head == null)
			throw new IllegalStateException("Can't remove from empty list");
		
		T r = head.data;
		head = head.next;
		size--;
		if(size == 0)
			head = tail = null;
		return r;
	}
	
	/**
	 * O(ind), updates a temporary variable ind times.
	 * @param ind
	 * @return
	 */
	public T remove(int ind){
		if(head == null)
			throw new IllegalStateException("Can't remove from empty list");
		else if(ind < 0 || ind >= size)
			throw new IllegalArgumentException("Index ouf of bounds.");
		else if(ind == 0)
			return removeHead();
		else{
			Node<T> tmp;
			for(tmp = head; ind > 1; tmp = tmp.next, ind--);
			
			if(tmp.next == tail)
				tail = tmp;
			
			T r = tmp.next.data;
			tmp.next = tmp.next.next;
			size--;
			return r;
		}
	}
	
	/**
	 * O(n) as this performs a linear search.
	 * @param val
	 * @return
	 */
	public T remove(T val){
		if(head == null)
			throw new IllegalStateException("Can't remove from empty list");
		else if(head.data.equals(val))
			return removeHead();
		else{
			Node<T> tmp;
			for(tmp = head; tmp != null && !tmp.next.data.equals(val); tmp = tmp.next);
			
			if(tmp == null)
				throw new IllegalArgumentException("Value not found in list!");
			else if(tmp.next == tail)
				tail = tmp;
			
			T r = tmp.next.data;
			tmp.next = tmp.next.next;
			size--;
			return r;
		}
	}
	
	public int size(){
		return size;
	}
	
	/**
	 * O(n) - this is a linear search.
	 * @param val
	 * @return
	 */
	public int indexOf(T val){
		int c = 0;
		for(Node<T> t = head; t != null && !t.data.equals(val); t = t.next, c++);
		if(c >= size)
			return -1;
		else
			return c;
	}
	
	public boolean isEmpty(){
		return size > 0;
	}
	
	public String toString(){
		String s = "";
		for(Node<T> t = head; t != null; t = t.next){
			s += t.data + " -> ";
		}
		return s;
	}
	
	//FOR MERGE SORT
	
	/**
	 * O(n): splits the list by looping once and updating
	 * a pointer to find the end. Another pointer is updated
	 * half the time. f(n) = 3*n/2 if pointer updates are the only things
	 * counted.
	 * @param src The linked list to divide.
	 * @return The second half. (src is updated to have only half its original contents.)
	 */
	public static <T> LinkedList<T> split(LinkedList<T> src){
		int stb = src.size/2;
		int stc = stb + (src.size % 2);
		
		if(stb == 0)
			return new LinkedList<T>();
		
		Node<T> mid = null;
		int c = 0;
		for(Node<T> s = src.head; s != null; s = s.next, c++)
			if(mid == null)
				mid = src.head;
			else if(c % 2 == 0)
				mid = mid.next;
		
		LinkedList<T> r = new LinkedList<T>();
		r.head = mid.next;
		r.tail = src.tail;
		src.tail = mid;
		mid.next = null;
		
		src.size = stc;
		r.size = stb;
		
		return r;
	}
	
	/**
	 * l and r must be sorted.
	 * Runs in O(n).
	 * @param l one of the linked lists to merge. Must be sorted.
	 * @param r the other linked list. Must be sorted.
	 * @return the sorted merger of both.
	 */
	public static <T extends Comparable<T>> LinkedList<T> merge(LinkedList<T> l, LinkedList<T> r){
		LinkedList<T> m = new LinkedList<T>();
		int ms = r.size + l.size;
		
		while(l.size() > 0 && r.size() > 0){
			int c = r.head.data.compareTo(l.head.data);
			if(c > 0)
				m.addToTail(l.removeHead());
			else
				m.addToTail(r.removeHead());
		}
		
		if(l.size() > 0){
			m.addToTail(l.removeHead());
			m.tail.next = l.head;
			m.tail = l.tail;
			l.head = l.tail = null;
			l.size = 0;
		}else if(r.size() > 0){
			m.addToTail(r.removeHead());
			m.tail.next = r.head;
			m.tail = r.tail;
			r.head = r.tail = null;
			r.size = 0;
		}
		
		m.size = ms;
		return m;
	}
	
	/**
	 * Sorts the list using merge sort.
	 * f(n) = (n + n)*lg n. The splitting and
	 * merging take O(n), making each level of the sort
	 * 2n. There are lg n levels. This is O(n lg n).
	 * @param s The list to sort.
	 * @return The sorted list.
	 */
	public static <T extends Comparable<T>> LinkedList<T> sort(LinkedList<T> s){
		if(s.size() > 1){
			LinkedList<T> r = split(s);
			r = sort(r);
			s = sort(s);
			return merge(r, s);
		}else{
			return s;
		}
	}
	
	public static void main(String [] args){
		LinkedList<Integer> test = new LinkedList<Integer>();
		for(int i = 0; i < 11; i++)
			test.addToTail((int)(Math.random()*100));
		
		System.out.println(test);
		
		LinkedList<Integer> s = sort(test);
		System.out.println(s);
		System.out.println(test);
	}
}
