package interpreter;

public interface PietIO {

	public int in(boolean isChar);
	public void out(int v, boolean charOut);
	public boolean debug(Coord curr, Direction dc, Direction cc);
	public void handleEnd(Coord end, Direction dc, Direction cc);
	public int popStack();
	public void pushStack(int v);
	public void rollStack(int rolls, int depth);
}
