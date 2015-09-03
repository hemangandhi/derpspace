package players;

import board.Location;

public abstract class BasePlayer implements LaserHandler {
	
	private Location loc;
	private Team team;
	private LaserDirection orientation;
	
	public BasePlayer(Location loc, Team t, LaserDirection o){
		team = t;
		move(loc, o);
		
	}

	public abstract PlayerMove[] getMoves();
	
	public void move(Location newLoc, LaserDirection newO){
		loc = newLoc;
		this.orientation = newO;
	}
	
	public void move(PlayerMove mv){
		loc = loc.getInDirection(mv);
		orientation = LaserDirection.next(orientation, mv.rotation());
	}
	
	public Team getTeam(){
		return team;
	}
}
