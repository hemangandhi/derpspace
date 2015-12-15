package trees;

import java.util.ArrayList;

public class Heap<T extends Comparable<T>> {

	private ArrayList<T> data;
	
	public Heap(){
		data = new ArrayList<T>();
	}
	
	public Heap(ArrayList<T> dat){
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
	}
	
	public void add(T v){
		if(data == null)
			data = new ArrayList<T>();
		data.add(v);
		
		for(int i = data.size() - 1, j = (i - 1)/2; j >= 0; i = j, j = (i - 1)/2 ){
			if(data.get(i).compareTo(data.get(j)) > 0){
				T t = data.get(i);
				data.set(i, data.get(j));
				data.set(j, t);
			}else{
				break;
			}
		}
	}
	
	public T remove(){
		if(data.size() == 1){
			return data.remove(0);
		}
		
		T m = data.get(0);
		data.set(0, data.remove(data.size() - 1));
		
		for(int p = 0, l = 2*p + 1, r = 2*p + 2; l < data.size(); l = 2*p + 1, r = 2*p + 2){
			int sc = (r >= data.size() || data.get(l).compareTo(data.get(r)) > 0)? l : r;
			if(data.get(p).compareTo(data.get(sc)) < 0){
				T tmp = data.get(p);
				data.set(p, data.get(sc));
				data.set(sc, tmp);
				
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
