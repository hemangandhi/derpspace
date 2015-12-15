package graphs;

import hashing.HashMap;
import trees.UpdateableHeap;
import linkedLists.LinkedList;

public class Graph<V> {
	
	private HashMap<V, HashMap<V, Integer>> adj;
	
	public void putLink(V src, V dest, int w){
		if(w < 0)
			throw new IllegalArgumentException("No negative edge weights!");
		
		if(adj.contains(src))
			adj.get(src).insert(dest, w);
		else{
			adj.insert(src, new HashMap<V, Integer>());
			putLink(src, dest, w);
		}
		
		if(!adj.contains(dest))
			adj.insert(dest, new HashMap<V, Integer>());
	}
	
	public int edgeWeight(V src, V dest){
		if(!adj.contains(src) || !adj.get(src).contains(dest))
			return -1;
		else
			return adj.get(src).get(dest);
	}
	
	public boolean pathExists(V src, V dest){
		//This is a DFS
		if(src == dest){
			return true;
		}else{
			for(V t: adj.get(src).keys())
				if(pathExists(t, dest))
					return true;
			
			return false;
		}
	}
	
	public HashMap<V, Integer> inDegs(){
		HashMap<V, Integer> r = new HashMap<V, Integer>();
		
		for(V srcs: adj.keys()){
			for(V dest: adj.get(srcs).keys()){
				if(r.contains(dest))
					r.insert(dest, r.get(dest) + 1);
				else
					r.insert(dest, 1);
			}
		}
		
		return r;
	}
	
	public V[] topSort(){
		HashMap<V, Integer> acc = new HashMap<V, Integer>();
		
		//Driver for DFS
		while(acc.size() < adj.size()){
			for(V v: adj.keys()){
				if(!acc.contains(v))
					topSort(v, acc);
			}
		}
		
		V[] r = (V []) new Object[adj.size()];
		for(V v: acc.keys()){
			r[acc.get(v)] = v;
		}
		
		return r;
	}
	
	private void topSort(V c, HashMap<V, Integer> acc){
		//DFS
		if(adj.get(c).size() == 0)
			acc.insert(c, adj.size() - acc.size());
		else{
			for(V d: adj.get(c).keys())
				topSort(d, acc);
		}
	}
	
	public LinkedList<V> dijkstrasSP(V src, V dest){
		class DijkstrasTuple implements Comparable<DijkstrasTuple>{
			public int w;
			public V dest;
			public LinkedList<V> path;

			public DijkstrasTuple(V src){
				path = new LinkedList<V>();
				this.dest = src;
			}
			
			public int compareTo(DijkstrasTuple o) {
				// TODO Auto-generated method stub
				return o.w - w;
			}
			
			public boolean equals(Object o){
				return dest == ((DijkstrasTuple)o).dest;
			}
			
			public int hashCode(){
				return dest.hashCode();
			}
		}
		
		HashMap<V, DijkstrasTuple> done = new HashMap<V, DijkstrasTuple>();
		UpdateableHeap<DijkstrasTuple> fringe = new UpdateableHeap<DijkstrasTuple>();
		done.insert(src, new DijkstrasTuple(src));
		V l = src;
		
		while(!done.contains(dest)){
			for(V n: adj.get(l).keys()){
				if(!done.contains(n)){
					DijkstrasTuple dt = new DijkstrasTuple(n);
					dt.w = done.get(l).w + edgeWeight(l, n);
					dt.path = done.get(l).path.copy();
					dt.path.addToTail(n);
					
					if(fringe.contains(dt)){
						DijkstrasTuple ot = fringe.get(dt);
						if(ot.compareTo(dt) < 0)
							fringe.update(ot, dt);
					}else{
						fringe.add(dt);
					}
				}
			}
			
			DijkstrasTuple ad = fringe.remove();
			l = ad.dest;
			done.insert(l, ad);
		}

		return done.get(dest).path;
	}
	
}
