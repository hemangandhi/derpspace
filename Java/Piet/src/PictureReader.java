import java.util.HashSet;
import java.awt.image.*;

public class PictureReader {
	
	public static enum Direction{
		UP(0,-1), DOWN(0, 1), LEFT(-1, 0), RIGHT(1, 0);
		
		private int x, y;
		
		private Direction(int dx, int dy){
			x = dx;
			y = dy;
		}
		
		public int[] fromPt(int px, int py){
			return new int[]{x + px, y + py};
		}
	}

	public static void read(BufferedImage src, PietIO io){
		
	}
	
	private static class Coord{
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
		
		public int x(){
			return x;
		}
		
		public int y(){
			return y;
		}
	}
	
	public static int blockSize(BufferedImage s, int x, int y){
		HashSet<Coord> c = new HashSet<Coord>(10, 3f);
		c.add(new Coord(x, y));
		blockCoords(s, x, y, c);
		return c.size();
	}
	
	private static void blockCoords(BufferedImage s, int x, int y, HashSet<Coord> acc){
		int m = s.getRGB(x, y);
		for(Direction d: Direction.values()){
			try{
				int [] nPt = d.fromPt(x, y);
				Coord n = new Coord(nPt[0], nPt[1]);
				if(s.getRGB(nPt[0], nPt[1]) == m && !acc.contains(n)){
					acc.add(n);
					blockCoords(s, nPt[0], nPt[1], acc);
				}
			}catch(ArrayIndexOutOfBoundsException aioobe){
				
			}
		}
	}
}
