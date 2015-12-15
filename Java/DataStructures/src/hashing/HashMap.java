package hashing;

import java.util.ArrayList;

public class HashMap<K,V> {

	private static class Node<K, V>{
		public K key;
		public V val;
		public Node<K,V> next;
		
		public Node(K k, V v, Node<K,V> n){
			key = k;
			val = v;
			next = n;
		}
	}
	
	private Node<K,V>[] left, right;
	private float lf;
	private int size, lInd;
	
	public HashMap(int size, float loadFactor){
		left = right = new Node[size];
		lf = loadFactor;
		lInd = -1;
	}
	
	public HashMap(){
		this(10, 0.75f);
	}
	
	public int size(){
		return size;
	}
	
	public void insert(K key, V val){
		remove(key);
		int l = key.hashCode() % right.length;

		right[l] = new Node<K, V>(key, val, right[l]);
		size++;	
		
		if(lInd >= 0){
			amortizedRehash();
		}else if(size/((double) right.length) >= lf){
			startRehash();
		}
		
	}
	
	private void amortizedRehash(){
		Node<K, V> add = left[lInd];
		left[lInd] = left[lInd].next;
		
		while(lInd < left.length && left[lInd] == null) lInd++;
		if(lInd >= left.length){
			lInd = -1;
			left = right;
		}	
		
		int r = add.key.hashCode() % right.length;
		for(Node<K, V> t = right[r]; t != null; t = t.next){
			if(t.key.equals(add.key))
				return;
		}
		
		add.next = right[r];
		right[r] = add;
	}
	
	private void startRehash(){
		right = new Node[right.length * 2];
		lInd = 0;
		while(lInd < left.length && left[lInd] == null) lInd++;
		
		if(lInd >= left.length){
			lInd = -1;
			left = right;
		}else
			amortizedRehash();
	}
	
	public V get(K key){
		int r = key.hashCode() % right.length;
		for(Node<K, V> t = right[r]; t != null; t = t.next)
			if(t.key.equals(key))
				return t.val;
		
		if(lInd >= 0){
			int l = key.hashCode() % left.length;
			for(Node<K, V> t = left[l]; t != null; t = t.next)
				if(t.key.equals(key))
					return t.val;
		}
		
		return null;
	}
	
	public ArrayList<K> keys(){
		while(lInd >= 0)
			amortizedRehash();
		
		ArrayList<K> r = new ArrayList<K>(right.length);
		
		for(int i = 0; i < right.length; i++){
			for(Node<K, V> t = right[i]; t != null; t = t.next){
				r.add(t.key);
			}
		}
		
		return r;
	}
	
	public V remove(K key){
		V r = null;
		if(lInd >= 0){
			int l = key.hashCode() % left.length;
			for(Node<K,V> t = left[l], p = null; t != null; p = t, t = t.next){
				if(key.equals(t.key)){
					r = t.val;
					size--;
					if(p == null){
						left[l] = t.next;
						while(lInd < left.length && left[lInd] == null) lInd++;
						if(lInd >= left.length){
							lInd = -1;
							left = right;
						}
					}else{
						p.next = t.next;
					}
				}
			}
		}
		
		int i = key.hashCode() % right.length;
		for(Node<K,V> t = right[i], p = null; t != null; p = t, t = t.next){
			if(key.equals(t.key)){
				r = t.val;
				size--;
				if(p == null)
					right[i] = t.next;
				else
					p.next = t.next;
			}
		}
		
		if(lInd >= 0)
			amortizedRehash();
		
		return r;
	}
	
	public boolean contains(K key){
		return get(key) != null;
	}
	
	public static void main(String [] args){
		HashMap<Integer, Integer> twos = new HashMap<Integer, Integer>();
		
		for(int i = 0; i < 90; i++)
			twos.insert(i, i * 2);
		
		System.out.println(twos.keys());
		
		System.out.println(twos.get(47));
	}
}
