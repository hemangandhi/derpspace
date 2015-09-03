package players;

public enum LaserDirection {

	NORTH,EAST,SOUTH,WEST;
	
	public static LaserDirection next(LaserDirection ld, int turns){
		return LaserDirection.values()[(ld.ordinal() + turns) % LaserDirection.values().length];
	}
}
