package players;

public enum PlayerMove {

	NORTH(0,-1,0), SOUTH(0,1,0), EAST(1,0,0),
	WEST(-1,0,0), NORTHEAST(1,-1,0), NORTHWEST(-1,-1,0),
	SOUTHEAST(1,1,0), SOUTHWEST(-1,1,0),
	CLOCKWISE(0,0,-1), COUNTERCLOCKWISE(0,0,1);
	
	private int x,y,r;
	
	private PlayerMove(int xa, int ya, int ra){
		x = xa; y = ya; r = ra;
	}
	
	public int[] dirVec(){
		return new int[]{x, y};
	}
	
	public int rotation(){
		return r;
	}
}
