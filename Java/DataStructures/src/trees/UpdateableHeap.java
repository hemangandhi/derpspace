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
	
	public int size(){
		return data.size();
	}
	
	public static <T extends Comparable<T>> void sort(ArrayList<T> s){
		Heap<T> disp = new Heap<T>(s);
		
		for(int i = s.size() - 1; i >= 0; i--){
			T t = s.get(0);
			s.set(0, s.get(i));
			s.set(i, t);
			
			for(int p = 0, l = 2*p + 1, r = 2*p + 2; l < i; l = p*2 + 1, r = 2*p + 2){
				int sc = (r >= i || s.get(l).compareTo(s.get(r)) > 0)? l : r;
				if(s.get(p).compareTo(s.get(sc)) < 0){
					T tmp = s.get(sc);
					s.set(sc,  s.get(p));
					s.set(p, tmp);
					
					p = sc;
				}else
					break;
			}
		}
	}
	
	public boolean contains(T v){
		return lst.contains(v);
	}
	
	public T get(T c){
		return data.get(lst.get(c));
	}
	
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
	}
	
	public static void main(String [] args){
		ArrayList<Integer> t = new ArrayList<Integer>();
		
		for(int i = 0; i < 100; i++){
			t.add((int)(Math.random()*100));
		}
		
		System.out.println(t);
		sort(t);
		System.out.println(t);
	}
}
