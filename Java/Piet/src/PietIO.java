
public interface PietIO {

	public int in(boolean isChar);
	public void out(boolean charOut);
	public int popStack();
	public void pushStack(int v);
	public void rollStack(int rolls, int depth);
}
