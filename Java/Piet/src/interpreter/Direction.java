package interpreter;

public enum Direction {
	RIGHT(1, 0), DOWN(0, 1), LEFT(-1, 0), UP(0, -1);
	
	private int x, y;
	
	private Direction(int dx, int dy){
		x = dx;
		y = dy;
	}
	
	public int[] fromPt(int px, int py){
		return new int[]{x + px, y + py};
	}
	
	public Coord fromPt(Coord pt){
		int [] xy = fromPt(pt.x(), pt.y());
		return new Coord(xy[0], xy[1]);
	}
	
	public Direction toThe(Direction o){
		int p = (o.ordinal() + 1) % 4;
		return fromOrdinal(p + this.ordinal());
	}
	
	public static Direction fromOrdinal(int ord){
		switch(ord % 4){
		case 0: return RIGHT;
		case 1: return DOWN;
		case 2: return LEFT;
		case 3: return UP;
		default: return null;//should never happen...
		}
	}
}
