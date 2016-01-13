package interpreter;

public class DirectionChooser {

	private int ptr;
	
	public Direction getDirection(){
		return Direction.values()[ptr];
	}
	
	public void point(int offset){
		if(offset < 0){
			offset *= -3;
		}
		
		ptr += offset;
		ptr %= Direction.values().length;
	}
}
