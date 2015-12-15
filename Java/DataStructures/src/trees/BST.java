package trees;

import java.util.ArrayList;

public class BST<T extends Comparable<T>> {

	private static class Node<U extends Comparable<U>>{
		public U data;
		public Node<U> left, right;
		
		public Node(U d, Node<U> l, Node<U> r){
			data = d;
			left = l;
			right = r;
		}
	}
	
	private Node<T> root;
	
	public void add(T data){
		if(root == null)
			root = new Node<T>(data, null, null);
		else{
			for(Node<T> t = root; t != null;){
				if(data.compareTo(t.data) > 0){
					if(t.right == null)
						t.right = new Node<T>(data, null, null);
					else
						t = t.right;
				}else{
					if(t.left == null)
						t.left = new Node<T>(data, null, null);
					else
						t = t.left;
				}
			}
		}
	}
	
	public T search(T key){
		for(Node<T> t = root; t != null;){
			int c = key.compareTo(t.data);
			if(c == 0)
				return t.data;
			else if(c < 0)
				t = t.left;
			else
				t = t.right;
		}
		
		return null;
	}
	
	public T delete(T key){
		Node<T> d, p = null;
		for(d = root; d != null;){
			int c = key.compareTo(d.data);
			if(c == 0)
				break;
			else if(c < 0){
				p = d;
				d = d.left;
			}else{
				p = d;
				d = d.right;
			}
		}
		
		if(d == null)
			throw new IllegalArgumentException("Key not found!");
		
		T r = d.data;
		if(d.left != null && d.right != null){
			Node<T> od = d;
			p = d;
			d = d.left;
			
			while(d.right != null){
				p = d;
				d = d.right;
			}
			
			od.data = d.data;
		}else if(d == root){
			if(d.left == null){
				d = d.right;
			}else{
				d = d.left;
			}

			p = root;
			root.data = d.data;
		}
		
		if(d.left == null){
			if(p.right == d)
				p.right = d.right;
			else
				p.left = d.right;
		}else{
			if(p.right == d)
				p.right = d.left;
			else
				p.left = d.left;
		}
		
		return r;
	}
	
	public ArrayList<T> inOrder(){
		ArrayList<T> r = new ArrayList<T>();
		inOrder(root, r);
		return r;
	}
	
	private void inOrder(Node<T> r, ArrayList<T> a){
		if(r != null){
			if(r.left != null)
				inOrder(r.left, a);
			
			a.add(r.data);
			
			if(r.right != null)
				inOrder(r.right, a);
		}
	}
}
