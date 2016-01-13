package interpreter;
import java.util.HashSet;
import java.util.HashMap;
import java.awt.image.*;

public class PictureReader {

	public static void read(BufferedImage src, PietIO io, boolean debug){
		DirectionChooser dc = new DirectionChooser();
		CodelChooser cc = new CodelChooser();
		Coord st, t;
		for(st = new Coord(0, 0), t = codelFrom(st, dc, cc, src);
				t != null && (!debug || io.debug(t, dc.getDirection(), cc.getDirection()));
				st = t, t = codelFrom(st, dc, cc, src)){
			PietColor pc = PietColor.byRGB(src.getRGB(st.x(), st.y())), pd = PietColor.byRGB(src.getRGB(t.x(), t.y())); 
			Commands.execute(Commands.byDiffs(PietColor.hueDiff(pc, pd), PietColor.lightnessDiff(pc, pd)), io, st, src, dc, cc);
		}
		io.handleEnd(t, dc.getDirection(), cc.getDirection());
	}
	
	public static int blockSize(BufferedImage s, Coord c){
		return blockSize(s, c.x(), c.y());
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
			int [] nPt = d.fromPt(x, y);
			Coord n = new Coord(nPt[0], nPt[1]);
			if(inBounds(n, s) && s.getRGB(nPt[0], nPt[1]) == m && !acc.contains(n)){
				acc.add(n);
				blockCoords(s, nPt[0], nPt[1], acc);
			}
		}
	}
	
	public static boolean inBounds(Coord s, BufferedImage img){
		return s.x() < img.getWidth() && s.y() < img.getHeight() && s.x() >= 0 && s.y() >= 0;
	}
	
	public static Coord codelFrom(Coord st, DirectionChooser dc, CodelChooser cc, BufferedImage img){
		PietColor stRGB = PietColor.byRGB(img.getRGB(st.x(), st.y()));
		if(stRGB == PietColor.WHITE)
			return codelFromWhite(st, dc, cc, img);
		
		int att = 0;
		do{
			Direction d = dc.getDirection();
			int ccu = 0;
			for(Coord t = st; inBounds(t, img); t = d.fromPt(t)){
				PietColor curr = PietColor.byRGB(img.getRGB(t.x(), t.y()));
				if(stRGB != curr){
					if(ccu == 0){
						t = d.toThe(Direction.DOWN).fromPt(t);
						d = d.toThe(cc.getDirection());
					}else if(ccu == 1){
						t = d.toThe(Direction.DOWN).fromPt(t);
						d = dc.getDirection();
					}else{
						if(curr == PietColor.WHITE)
							return codelFromWhite(t, dc, cc, img);
						else if(PietColor.isRestricted(curr))
							break;
						else
							return t;
					}
					ccu++;
				}	
			}
			
			if(att % 2 == 0)
				cc.point(1);
			else
				dc.point(1);
			att++;
		}while(att < 8);
		
		return null;
	}
	
	private static Coord codelFromWhite(Coord st, DirectionChooser dc, CodelChooser cc, BufferedImage img){
		System.out.println("WHITE!");
		
		HashMap<Coord, Direction> visits = new HashMap<>();
		Coord t = st;
		do{
			visits.put(t, dc.getDirection());
			for(;inBounds(t, img) && PietColor.byRGB(img.getRGB(t.x(), t.y())) == PietColor.WHITE; t = dc.getDirection().fromPt(t));
			if(!inBounds(t, img) || PietColor.isRestricted(img.getRGB(t.x(), t.y()))){
				t = dc.getDirection().toThe(Direction.DOWN).fromPt(t);
				dc.point(1);
				cc.point(1);
			}else
				return t;
		}while(!visits.containsKey(t) || visits.get(t) != dc.getDirection());
		
		return null;
	}
}