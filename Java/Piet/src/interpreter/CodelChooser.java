package interpreter;

public class CodelChooser {

	private int pt;
	
	public Direction getDirection(){
		return Direction.values()[pt];
	}
	
	public void point(int offset){
		offset = Math.abs(offset)*2;
		pt = (pt + offset) % Direction.values().length;
	}
}
