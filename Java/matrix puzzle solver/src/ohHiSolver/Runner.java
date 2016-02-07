package ohHiSolver;

import swingUI.*;

public class Runner {

	public static void main(String [] args){
		GeneralFrame<CellState> f = new GeneralFrame<>(new OhHiSolver());
	}
}
