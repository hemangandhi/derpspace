package trees;

import java.util.ArrayList;
import hashing.HashMap;

public class UpdateableHeap<T extends Comparable<T>> {

	private ArrayList<T> data;
	private HashMap<T, Integer> lst;
	
	public UpdateableHeap(){
		data = new ArrayList<T>();
		lst = new HashMap<T, Integer>();
	}
	
	/**
	 * Heapifies dat in places.
	 * O(n)
	 * @param dat the data
	 */
	public UpdateableHeap(ArrayList<T> dat){
		for(int i = (dat.size() - 1)/2; i >= 0; i--){
			for(int p = i, l = 2*p + 1, r = 2*p+2; l < dat.size(); l = 2*p+1, r = 2*p+2){
				int sc = (r >= dat.size() || dat.get(l).compareTo(dat.get(r)) > 0)? l : r;
				if(dat.get(p).compareTo(dat.get(sc)) < 0){
					T tmp = dat.get(p);
					dat.set(p, dat.get(sc));
					dat.set(sc, tmp);
					
					p = sc;
				}else{
					break;
				}
			}
		}
		
		data = dat;
		
		lst = new HashMap<T, Integer>();
		for(int i = 0; i < dat.size(); i++){
			lst.insert(dat.get(i), i);
		}
	}
	
	private void swap(int i, int j){
		T ai = data.get(i), aj = data.get(j);
		lst.insert(ai, j);
		lst.insert(aj, i);
		
		data.set(i, aj);
		data.set(j, ai);
	}
	
	/**
	 * Adds value to heap, updates hash table.
	 * @param v the value to add
	 */
	public void add(T v){
		data.add(v);
		
		for(int i = data.size() - 1, j = (i - 1)/2; j >= 0; i = j, j = (i - 1)/2 ){
			if(data.get(i).compareTo(data.get(j)) > 0){
				swap(i, j);
			}else{
				break;
			}
		}
	}
	
	/**
	 * Removes a value from the heap.
	 * @return the removed value.
	 */
	public T remove(){
		if(data.size() == 1){
			lst.remove(data.get(0));
			return data.remove(0);
		}
		
		T m = data.get(0);
		data.set(0, data.remove(data.size() - 1));
		lst.remove(m);
		lst.insert(data.get(0), 0);
		
		for(int p = 0, l = 2*p + 1, r = 2*p + 2; l < data.size(); l = 2*p + 1, r = 2*p + 2){
			int sc = (r >= data.size() || data.get(l).compareTo(data.get(r)) > 0)? l : r;
			if(data.get(p).compareTo(data.get(sc)) < 0){
				swap(p, sc);
				
				p = sc;
			}else{
				break;
			}
		}
		
		return m;
	}
	
	/**
	 * Gets the size.
	 * @return the size.
	 */
	public int size(){
		return data.size();
	}
	
	/**
	 * Checks if a value is contained in the heap.
	 * O(1) thanks to hash table.
	 * @param v the value to test.
	 * @return whether the value is in the heap.
	 */
	public boolean contains(T v){
		return lst.contains(v);
	}
	
	/**
	 * Gets the value from the heap.
	 * The value of returned should not be altered.
	 * O(1). 
	 * @param c the value to get.
	 * @return the value gotten.
	 */
	public T get(T c){
		return data.get(lst.get(c));
	}
	
	/**
	 * Updates a value in the heap.
	 * O(log n). Performs both sifts in case
	 * old > ne or old < ne. O(1) if old == ne.
	 * @param old the old value.
	 * @param ne the updated value.
	 */
	public void update(T old, T ne){
		int ind = lst.remove(old);
		lst.insert(ne, ind);
		
		data.set(ind, ne);
		
		for(int i = ind, j = (i - 1)/2; j >= 0; i = j, j = (i - 1)/2 ){
			if(data.get(i).compareTo(data.get(j)) > 0){
				swap(i, j);
			}else{
				break;
			}
		}
		
		ind = lst.get(ne);
		for(int p = ind, l = 2*p + 1, r = 2*p + 2; l < data.size(); l = 2*p + 1, r = 2*p + 2){
			int sm = (r >= data.size() || data.get(l).compareTo(data.get(r)) > 0)? l : r;
			if(data.get(sm).compareTo(data.get(p)) > 0){
				swap(p, sm);
				p = sm;
			}else{
				break;
			}
		}
	}
}
