package board;

public class Board {

	private Location[][] bd;
	
	public Location consult(int x, int y){
		if(y >= 0 && y < bd.length && x >= 0 && x < bd[0].length)
			return bd[y][x];
		else
			return null;
	}
	
	public int[] getDims(){
		return new int[]{bd[0].length, bd.length};
	}
}
