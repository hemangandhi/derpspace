package apps;

import hashing.HashMap;
import trees.Heap;

public class HuffmanCoder {

	private static class Node implements Comparable<Node>{
		public float p;
		public char v;
		public Node left, right;
		
		public Node(char c, float p){
			this.p = p;
			v = c;
		}
		
		public Node(Node l, Node r){
			p = l.p + r.p;
			v = 0;
			left = l;
			right = r;
		}
		
		public int compareTo(Node n){
			return -1*((Float)p).compareTo((Float)n.p);
		}
	}
	
	/**
	 * Gets all the leaf nodes for the Huffman tree.
	 * O(n) if n is the string length.
	 * @param toEnc the string to encode.
	 * @return the leaf nodes for the Huffman tree.
	 */
	public static Heap<Node> initNodes(String toEnc){
		HashMap<Character, Integer> charC = new HashMap<Character, Integer>();
		for(char c: toEnc.toCharArray()){
			if(charC.contains(c))
				charC.insert(c, charC.get(c) + 1);
			else
				charC.insert(c, 1);
		}
		
		Heap<Node> r = new Heap<Node>();
		for(Character c: charC.keys()){
			r.add(new Node(c, charC.get(c)/((float) toEnc.length())));
		}
		
		return r;
	}
	
	/**
	 * Builds a Huffman tree.
	 * O(n) if n is the number of leaf nodes.
	 * @param nodes the leaf nodes.
	 * @return the root of the Huffman tree.
	 */
	public static Node code(Heap<Node> nodes){
		Heap<Node> t = new Heap<Node>();
		while(true){
			Node t1 = min(nodes, t), t2 = min(nodes, t);
			
			if(t2 == null)
				return t1;
			else
				t.add(new Node(t1, t2));
		}
	}
	
	private static Node min(Heap<Node> l, Heap<Node> r){
		if(l.size() == 0 && r.size() == 0)
			return null;
		else if(l.size() == 0)
			return r.remove();
		else if(r.size() == 0)
			return l.remove();
		else{
			Node m = l.remove(), s = r.remove();
			if(m.compareTo(s) > 0){
				r.add(s);
				return m;
			}else{
				l.add(m);
				return s;
			}
		}
	}
	
	/**
	 * Gets the encoded characters.
	 * O(n) as this is a pre-order traversal of the
	 * Huffman tree. O(N) overall, if N is the string's length.
	 * @param arg the string to encode.
	 * @return the encoded characters.
	 */
	public static HashMap<Character, String> encoded(String arg){
		Node hTree = code(initNodes(arg));
		HashMap<Character, String> r = new HashMap<Character, String>();
		encoded(hTree, "", r);
		return r;
	}
	
	public static void encoded(Node root, String bits, HashMap<Character, String> acc){
		if(root == null)
			return;
		else if(root.left == null && root.right == null){
			acc.insert(root.v, bits);
		}else{
			if(root.right != null)
				encoded(root.right, bits + "1", acc);
			if(root.left != null)
				encoded(root.left, bits + "0", acc);
		}
	}
	
	/**
	 * Decodes a Huffman encoded string given a tree.
	 * O(N) where N is the length of the encoded string.
	 * @param enc the string to encode.
	 * @param root the root of the Huffman tree.
	 * @return the decoded string.
	 */
	public static String decode(String enc, Node root){
		String ret = "";
		Node t = root;
		for(int i = 0; i < enc.length(); i++){
			if(enc.charAt(i) == '1')
				t = t.right;
			else
				t = t.left;
			
			if(t.left == null && t.right == null){
				ret += t.v;
				t = root;
			}	
		}
		
		return ret;
	}
	
	public static String encode(String enc){
		HashMap<Character, String> bits = encoded(enc);
		String r = "";
		for(char c: enc.toCharArray())
			r += bits.get(c);
		
		return r;
	}
	
	public static void main(String [] args){
		String test = "aaaaaaaaaaaaabd";
		String e = encode(test);
		System.out.println(e);
		
		System.out.println(decode(e,code(initNodes(test))));
	}
}
