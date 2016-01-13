package interpreter;
public class Coord {
	private int x, y;
	
	public Coord(int x, int y){
		this.x = x;
		this.y = y;
	}
	
	public int hashCode(){
		return (x + "" + y).hashCode();
	}
	
	public boolean equals(Object o){
		Coord other = (Coord) o;
		return this.x == other.x && this.y == other.y;
	}
	
	public String toString(){
		return "(" + x + ", " + y + ")";
	}
	
	public int x(){
		return x;
	}
	
	public int y(){
		return y;
	}
}
