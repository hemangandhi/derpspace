package interpreter;
import java.awt.image.BufferedImage;


public enum Commands {

	ADD(1, 0), DIVIDE(2, 0), GREATER(3, 0), DUPLICATE(4, 0), CHAR_IN(5, 0),
	PUSH(0, 1), SUBTRACT(1, 1), MOD(2, 1), POINTER(3, 1), ROLL(4,1), INT_OUT(5, 1),
	POP(0, 2), MULTIPLY(1, 2), NOT(2, 2), SWITCH(3, 2), INT_IN(4, 2), CHAR_OUT(5, 4);
	
	private int dh, dl;
	
	private Commands(int h, int l){
		dh = h; dl = l;
	}
	
	public static Commands byDiffs(int dh, int dl){
		for(Commands c: Commands.values())
			if(c.dh == dh && c.dl == dl)
				return c;
		
		return null;
	}
	
	public static void execute(Commands c, PietIO io, Coord pt, BufferedImage img, DirectionChooser dc, CodelChooser cc){
		switch(c){
		case PUSH:
			io.pushStack(PictureReader.blockSize(img, pt));
			break;
		case POP:
			io.popStack();
			break;
		case ADD:
			io.pushStack(io.popStack() + io.popStack());
			break;
		case SUBTRACT:
			io.pushStack(-1*(io.popStack() - io.popStack()));
			break;
		case MULTIPLY:
			io.pushStack(io.popStack()*io.popStack());
			break;
		case DIVIDE:
			int n = io.popStack(), d = io.popStack();
			if(n != 0)
				io.pushStack(d/n);
			break;
		case MOD:
			int m = io.popStack(), e = io.popStack();
			if(m != 0)
				io.pushStack(e % m);
			break;
		case NOT:
			if(io.popStack() == 0)
				io.pushStack(1);
			else
				io.pushStack(0);
			break;
		case GREATER:
			int l = io.popStack(), f = io.popStack();
			if(f > l)
				io.pushStack(1);
			else
				io.pushStack(0);
			break;
		case POINTER:
			dc.point(io.popStack());
			break;
		case SWITCH:
			cc.point(io.popStack());
			break;
		case DUPLICATE:
			int aa = io.popStack();
			io.pushStack(aa);io.pushStack(aa);
			break;
		case ROLL:
			io.rollStack(io.popStack(), io.popStack());
			break;
		case CHAR_IN:
			io.pushStack(io.in(true));
			break;
		case INT_IN:
			io.pushStack(io.in(false));
			break;
		case CHAR_OUT:
			io.out(io.popStack(), true);
			break;
		case INT_OUT:
			io.out(io.popStack(), false);
			break;
		}
	}
}
