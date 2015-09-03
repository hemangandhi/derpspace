package board;

import players.*;

public class Location {

	private int x,y;
	private BasePlayer occupant;
	private Board env;
	
	public Location(int x, int y, BasePlayer occupant, Board environ){
		this.x = x;
		this.y = y;
		this.occupant = occupant;
		this.env = environ;
	}
	
	public BasePlayer getOccupant(){
		return occupant;
	}
	
	public Location getInDirection(PlayerMove dir){
		int [] vec = dir.dirVec();
		if(vec[0] + vec[1] == 0)
			return this;
		Location adj = env.consult(vec[0] + x, vec[1] + y);
		return adj;
	}
	
	
}
