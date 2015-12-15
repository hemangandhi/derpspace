package trees;

import java.util.ArrayList;

public class BinaryTree<T> {

	private static class Node<U>{
		public U data;
		public Node<U> left, right;
		
		public Node(U d, Node<U> l, Node<U> r){
			data = d;
			left = l;
			right = r;
		}
	}
	
	private Node<T> root;
	
	public BinaryTree(ArrayList<T> preOrder, ArrayList<T> inOrder){
		root = makeBTree(preOrder, inOrder, 0, 0, inOrder.size());
	}
	
	public static <T> int indexIn(ArrayList<T> ar, T v, int l, int r){
		for(int i = l; i < r; i++){
			if(ar.get(i).equals(v))
				return i;
		}
		
		return -1;
	}
	
	private static <T> Node<T> makeBTree(ArrayList<T> preOrder, ArrayList<T> inOrder, int ps, int is, int ie){
		if(ps >= preOrder.size())
			return null;
		int spl = indexIn(inOrder,preOrder.get(ps),is,ie);
		return new Node<T>(preOrder.get(ps),makeBTree(preOrder, inOrder, ps + 1, is, spl),makeBTree(preOrder, inOrder, ps + (spl - is), spl, ie));
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
}
