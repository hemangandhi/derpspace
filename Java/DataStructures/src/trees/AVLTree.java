package trees;

import java.util.ArrayList;

public class AVLTree<T extends Comparable<T>> {
	
	private static class Node<U extends Comparable<U>>{
		public U data;
		public Node<U> left, right;
		public int lh, rh;
		
		public Node(U d){
			data = d;
		}
		
		public void computeHeights(){
			if(left == null)
				lh = 0;
			else
				lh = 1 + Math.max(left.lh, left.rh);
			
			if(right == null)
				rh = 0;
			else
				rh = 1 + Math.max(right.lh, right.rh);
		}
	}
	
	private Node<T> root;
	
	public T search(T key){
		for(Node<T> t = root; t != null;){
			int c = t.data.compareTo(key);
			if(c == 0)
				return t.data;
			else if(c < 0)
				t = t.right;
			else
				t = t.left;
		}
		
		return null;
	}
	
	public void insert(T data){
		if(root == null)
			root = new Node<T>(data);
		else
			root = insert(root, data);
	}
	
	private Node<T> insert(Node<T> r, T data){
		int c = data.compareTo(r.data);
		if(c == 0)
			throw new IllegalArgumentException("No duplicate keys!");
		else if(c < 0){
			if(r.left != null){
				r.left = insert(r.left, data);
			}else{
				r.left = new Node<T>(data);
				r.lh++;
			}
		}else{
			if(r.right != null){
				r.right = insert(r.right, data);
			}else{
				r.right = new Node<T>(data);
				r.rh++;
			}
		}
		
		return rebalance(r);
	}
	
	private Node<T> rebalance(Node<T> r){
		r.computeHeights();
		
		if(r.lh - r.rh > 1){
			if(r.left.lh > r.left.rh){
				Node<T> t = r.left;
				r.left = t.right;
				t.right = r;
				
				r.computeHeights();
				t.computeHeights();
				
				return t;
			}else{
				Node<T> t = r.left.right;
				r.left.right = t.left;
				t.left = r.left;
				r.left = t.right;
				t.right = r;
				
				t.right.computeHeights();
				t.left.computeHeights();
				t.computeHeights();
				
				return t;
			}
		}else if(r.rh - r.lh > 1){
			if(r.right.rh > r.right.lh){
				Node<T> t = r.right;
				r.right = t.left;
				t.left = r;
				
				r.computeHeights();
				t.computeHeights();
				
				return t;
			}else{
				Node<T> t = r.right.left;
				r.right.left = t.right;
				t.right = r.right;
				r.right = t.left;
				t.left = r;
				
				t.right.computeHeights();
				t.left.computeHeights();
				t.computeHeights();
				
				return t;
			}
		}
		
		return r;
	}
	
	public ArrayList<T> inOrder(){
		ArrayList<T> r = new ArrayList<T>();
		inOrder(root, r);
		return r;
	}
	
	private void inOrder(Node<T> r, ArrayList<T> acc){
		if(r != null){
			if(r.left != null)
				inOrder(r.left, acc);
			
			acc.add(r.data);
			
			if(r.right != null)
				inOrder(r.right, acc);
		}
	}
	
	public T delete(T key){
		T r = search(key);
		if(r == null)
			throw new IllegalArgumentException("Key not found!");
		
		root = delete(root, key);
		
		return r;
	}
	
	private Node<T> delete(Node<T> r, T key){
		int c = key.compareTo(r.data);
		if(c == 0){
			if(r.left != null && r.right != null){
				Node<T> p = r, t = r.left;
				while(t.right != null){
					p = t;
					t = t.right;
				}
				
				r.data = t.data;
				if(p == r)
					r.left = r.left.left;
				else	
					p.right = delete(t, t.data);
			}else if(r.left == null){
				return r.right;
			}else{
				return r.left;
			}
		}else if(c < 0){
			r.left = delete(r.left, key);
		}else{
			r.right = delete(r.right, key);
		}
		
		return rebalance(r);
	}
	
	public static <T extends Comparable<T>> ArrayList<T> sort(ArrayList<T> arg){
		AVLTree<T> a = new AVLTree<T>();
		
		for(T t: arg){
			a.insert(t);
		}
		
		return a.inOrder();
	}
	
	public static void main(String [] args){
		AVLTree<Integer> avl = new AVLTree<Integer>();
		
		for(int i = 0; i < 10; i++)
			avl.insert(i*10);
		
		avl.insert(5);
		avl.insert(3);
		
		avl.delete(3);
		avl.delete(0);
		avl.delete(5);
		
		avl.delete(20);
		
		System.out.println(avl.inOrder());
	}
}
