package zeroHH1;

import java.util.Stack;

public class Cell {

	public static enum CellState{BLANK, RED, BLUE};
	
	private static class StateTuple{
		public CellState cs;
		public int modlevel;
		
		public StateTuple(CellState c, int l){
			cs = c;
			modlevel = l;
		}
	}
	
	private Stack<StateTuple> mem;
	
	public Cell(CellState state){
		mem = new Stack<>();
		mem.push(new StateTuple(state, 0));
	}
	
	public void putState(CellState state, int level){
		mem.push(new StateTuple(state, level));
	}
	
	public CellState getState(){
		return mem.peek().cs;
	}
	
	public int modifications(){
		return mem.size();
	}
	
	public CellState resetState(){
		return mem.pop().cs;
	}
	
	public void resetToBefore(int level){
		while(mem.peek().modlevel >= level)
			mem.pop();
	}
}
