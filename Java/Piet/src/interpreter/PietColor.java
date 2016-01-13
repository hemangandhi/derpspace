package interpreter;
import java.awt.Color;

public enum PietColor {

	LIGHT_RED(0xFFC0C0), LIGHT_YELLOW(0xFFFFC0), LIGHT_GREEN(0xC0FFC0), LIGHT_CYAN(0xC0FFFF), LIGHT_BLUE(0xC0C0FF), LIGHT_MAGENTA(0xFFC0FF),
	RED(0xFF000), YELLOW(0xFFFF00), GREEN(0x00FF00), CYAN(0x00FFFF), BLUE(0x0000FF), MAGENTA(0xFF00FF),
	DARK_RED(0xC00000), DARK_YELLOW(0xC0C000), DARK_GREEN(0x00C000), DARK_CYAN(0x00C0C0), DARK_BLUE(0x0000C0), DARK_MAGENTA(0xC000C0),
	BLACK(0), WHITE(0xFFFFFF);
	
	private Color c;
	
	private PietColor(int rgb){
		c = new Color(rgb);
	}
	
	public static PietColor byRGB(int rgb){
		Color t = new Color(rgb);
		for(PietColor pc: PietColor.values())
			if(pc.c.equals(t))
				return pc;
		
		System.out.println("CNF: " + rgb);
		return null;
	}
	
	public static boolean isRestricted(int rgb){
		return isRestricted(byRGB(rgb));
		
	}
	
	public static boolean isRestricted(PietColor pc){
		return pc == BLACK || pc == null;
	}
	
	public static boolean isNormal(PietColor pc){
		return !isRestricted(pc) && pc != WHITE;
	}
	
	public static boolean isNormal(int rgb){
		return isNormal(byRGB(rgb));
	}
	
	public static int lightnessDiff(PietColor a, PietColor b){
		if(!isNormal(a) || !isNormal(b))
			throw new IllegalArgumentException("No BLACK, WHITE or null, please!");
		
		int x = a.ordinal()/6, y = b.ordinal()/6;
		return (y - x) % 3;
	}
	
	public static int hueDiff(PietColor a, PietColor b){
		if(!isNormal(a) || !isNormal(b))
			throw new IllegalArgumentException("No BLACK, WHITE or null, please!");
		
		int x = a.ordinal() % 6, y = b.ordinal() % 6;
		return (y - x) % 6;
	}
}
