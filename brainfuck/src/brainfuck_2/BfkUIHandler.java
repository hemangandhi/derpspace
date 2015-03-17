package brainfuck_2;

import java.util.HashMap;

/**
 * Interface for all required user interaction with a bfk interpreter.
 * @author Heman
 *
 */
public interface BfkUIHandler {

	/**
	 * Handle the output '.'
	 * @param val the value to output
	 */
	void output(int val);
	
	/**
	 * Handle the input ','
	 * @return the value inputed
	 */
	int getInput();
	
	/**
	 * Handle the debug-like run of code
	 * @param tape the memory tape
	 * @param pointer the pointer to the tape
	 * @param readIndex the index in the code
	 * @return true to keep debugging
	 */
	boolean debug(HashMap<Integer,Integer> tape, int pointer, int readIndex);
	
	/**
	 * Display an error message for infinite loops
	 */
	void showError();
	
	/**
	 * Let the UI handle the end of the code
	 */
	void endOfCode();
}
