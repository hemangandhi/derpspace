package apps;

import java.util.ArrayList;
import linkedLists.LinkedList;

public class Sorter {

	public static <T extends Comparable<T>> void quickSort(ArrayList<T> arg){
		quickSort(arg, 0, arg.size() - 1);
	}
	
	private static <T extends Comparable<T>> void quickSort(ArrayList<T> arg, int l, int r){
		if(r - l >= 1){
			int p = split(arg, l, r);
			quickSort(arg, l, p - 1);
			quickSort(arg, p + 1, r);
		}
	}
	
	private static <T extends Comparable<T>> int split(ArrayList<T> arg, int l, int r){
		int piv = l;
		int i = l + 1;
		int j = r;
		while(i <= j){
			while(i <= r && i <= j && arg.get(i).compareTo(arg.get(piv)) < 0) i++;
			while(j >= l + 1 && j >= i && arg.get(j).compareTo(arg.get(piv)) > 0) j--;
			
			if(i <= j){
				T t = arg.get(i);
				arg.set(i, arg.get(j));
				arg.set(j, t);
				i++;
				j--;
			}
		}
		
		T t = arg.get(j);
		arg.set(j, arg.get(piv));
		arg.set(piv, t);
		return j;
	}
	
	public static <T extends Comparable<T>> boolean isSorted(ArrayList<T> arg){
		for(int i = 1; i < arg.size(); i++){
			if(arg.get(i - 1).compareTo(arg.get(i)) > 0)
				return false;
		}
		
		return true;
	}
	
	public static interface Radixator<T>{
		public int atRadix(T val, int radix);
		public int getBase(T val);
	}
	
	public static <T> ArrayList<T> radixSort(ArrayList<T> arg, Radixator<T> rad){
		LinkedList<T>[] hm = new LinkedList[rad.getBase(arg.get(0))];
		for(int i = 0; i < hm.length; i++)
			hm[i] = new LinkedList<T>();
		
		LinkedList<T> res = new LinkedList<T>();
		LinkedList<T> tmp = new LinkedList<T>();
		for(T a: arg){
			tmp.addToTail(a);
		}
		
		int r = 0;
		while(res.size() < arg.size()){
			while(tmp.size() > 0){
				T t = tmp.removeHead();
				int h = rad.atRadix(t, r);
				if(h < 0){
					res.addToTail(t);
				}else
					hm[h].addToTail(t);
			}
			
			int i;
			for(i = 0; i < hm.length && hm[i].size() < 1; i++);
			if(i >= hm.length)
				break;
			tmp = hm[i];
			hm[i] = new LinkedList<T>();
			for(; i < hm.length; i++){
				tmp.join(hm[i]);
			}
			
			r++;
		}
		
		return res.toArrayList();
	}
	
	public static void testQS(){
		ArrayList<Integer> t = new ArrayList<Integer>(100);
		
		for(int i = 0; i < 100; i++)
			t.add((int)(Math.random()*100));
		
		System.out.println(t);
		quickSort(t);
		System.out.println(t);
		System.out.println(isSorted(t));		

	}
	
	public static void testRadix(){
		Radixator<Integer> rad = new Radixator<Integer>(){
			public int atRadix(Integer val, int r){
				if(val >= Math.pow(10,  r))
					return (int) (val / Math.pow(10, r)) % 10;
				else
					return -1;
			}
			
			public int getBase(Integer v){
				return 10;
			}
		};
		
		ArrayList<Integer> t = new ArrayList<Integer>(100);
		
		for(int i = 0; i < 100; i++)
			t.add((int)(Math.random()*100));
		
		System.out.println(t);
		t = radixSort(t, rad);
		System.out.println(t);
		System.out.println(isSorted(t));
	}
	
	public static void main(String[] args){
		testRadix();	
	}
}
