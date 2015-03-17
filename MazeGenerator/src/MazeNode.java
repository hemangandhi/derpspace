import java.util.*;

public class MazeNode {

	private ArrayList<MazeNode> neighbours;
	private ArrayList<MazeNode> connections;
	
	private int x, y;
	
	public MazeNode(int x, int y){
		this.x = x;
		this.y = y;
		neighbours = new ArrayList<MazeNode>();
		connections = new ArrayList<MazeNode>();
	}
	
	public boolean isConnected(MazeNode other){
		ArrayList<MazeNode> visits = new ArrayList<MazeNode>();
		visits.add(this);
		return isConnected(other,visits);
	}
	
	public boolean isConnected(MazeNode other, ArrayList<MazeNode> visiteds){
		visiteds.add(this);
		for(MazeNode node: connections){
			if(node == other)
				return true;
			else if(!visiteds.contains(node) && node.isConnected(other,visiteds))
				return true;
		}
		return false;
	}
	
	public int getX(){
		return x;
	}
	
	public int getY(){
		return y;
	}
	
	public void addNieghbour(MazeNode toAdd){
		neighbours.add(toAdd);
	}
	
	public void connect(MazeNode other){
		if(neighbours.contains(other)){
			connections.add(other);
			neighbours.remove(other);
		}	
		
		if(!other.isConnected(this))
			other.connect(this);
	}
	
	public MazeNode getRandomDisconnectedNeighbour() throws Exception{
		ArrayList<MazeNode> disconnecteds = new ArrayList<MazeNode>();
		for(MazeNode ne: neighbours){
			if(!isConnected(ne))
				disconnecteds.add(ne);
		}
		if(disconnecteds.size() == 0)
			throw new Exception("Backtrack!");
		return disconnecteds.get((int)(Math.random()*disconnecteds.size()));
	}
	
	public MazeNode getRandomConnection(){
		return connections.get((int)(Math.random()*connections.size()));
	}
	
	public ArrayList<MazeNode> connecteds(){
		return connections;
	}
}
