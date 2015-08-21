package com.heman.brainfuckapp;

import java.util.HashMap;

/**
 * Brainfuck interpreter for any type of interface.
 * Expects a BfkIOHandler to handle the user interaction.
 * @author Heman
 *
 */
public class ReadBfk {

	/**
	 * Read the entire code, running everything.
	 * @param code the code to read.
	 * @param bih the IO handler to send data to.
	 */
	public static void readAll(String code, BfkUIHandler bih){
		//The memory tape
		HashMap<Integer,Integer> tape = new HashMap<Integer,Integer>();
		//Store index in tape and number of loop executions
		int index = 0, loops = 0;
		
		//Loop through the entire code
		for(int i = 0; i < code.length(); i++, loops++){
			//Check the current char
			switch(code.charAt(i)){
			
			//If plus add a value to the memory
			case '+':
				if(tape.containsKey(index))
					tape.put(index, tape.get(index) + 1);
				else
					//Insert the value as required
					tape.put(index,1);
				break;
				
			//If minus, take one away.
			//Garbage collects 0's and bans negatives	
			case '-':
				if(tape.containsKey(index)){
					tape.put(index, tape.get(index) - 1);
					if(tape.get(index) == 0)
						tape.remove(index);
				}
				break;
				
			//Shift the pointer right (infinitely)	
			case '>':
				index++;
				break;
				
			//Shift the pointer left (allows negatives)	
			case '<':
				index--;
				break;
				
			//Enter a loop (if the value is non-zero)	
			case '[':
				if(!tape.containsKey(index) || tape.get(index) == 0){
					//Move to ']' in case of a zero
					int ins = 1;
					while(i < code.length() && ins > 0){
						i++;
						if(code.charAt(i) == '[')
							ins++;
						else if(code.charAt(i) == ']')
							ins--;
					}	
				}
				break;
				
			//Loop back if the value is non-zero.	
			case ']':
				if(tape.containsKey(index) && tape.get(index) != 0){
					//Goes to '['
					int outs = 1;
					while( i > 0 && outs > 0){
						i--;
						if(code.charAt(i) == ']')
							outs++;
						else if(code.charAt(i) == '[')
							outs--;
					}	
				}
				break;
				
			//Handle output	
			case '.':
				if(tape.containsKey(index))
					bih.output(tape.get(index));
				else
					bih.output(0);
				break;
				
			//Handle input	
			case ',':
				tape.put(index,bih.getInput());
				break;
			}
			//All others characters are ignored
			
			//Check for nearly infinite loops, alert user.
			if(loops > code.length()*10000){
				bih.showError();
				//Exit loop
				break;
			}	
		}
		
		bih.endOfCode();
		
	}
	
	/**
	 * Run a debug-like interpretation of the code.
	 * @param code the code to use.
	 * @param bih the IO handler.
	 */
	public static void debug(String code, BfkUIHandler bih){
		//Works exactly like the other method with two key differences:
		//Before executing the command, the handler's debug is called.
		//There is no infinite loop prevention - the user controls execution.
		HashMap<Integer,Integer> tape = new HashMap<Integer,Integer>();
		int index = 0;
		
		for(int i = 0; i < code.length() && bih.debug(tape, index, i); i++){
			switch(code.charAt(i)){
			
			case '+':
				if(tape.containsKey(index))
					tape.put(index, tape.get(index) + 1);
				else
					tape.put(index,1);
				break;
				
			case '-':
				if(tape.containsKey(index)){
					tape.put(index, tape.get(index) - 1);
					if(tape.get(index) == 0)
						tape.remove(index);
				}
				break;
				
			case '>':
				index++;
				break;
				
			case '<':
				index--;
				break;
				
			case '[':
				if(!tape.containsKey(index) || tape.get(index) == 0){
					int ins = 1;
					while(i < code.length() && ins > 0){
						i++;
						if(code.charAt(i) == '[')
							ins++;
						else if(code.charAt(i) == ']')
							ins--;
					}
				}
				break;
				
			case ']':
				if(tape.containsKey(index) && tape.get(index) != 0){
					int outs = 1;
					while( i > 0 && outs > 0){
						i--;
						if(code.charAt(i) == ']')
							outs++;
						else if(code.charAt(i) == '[')
							outs--;
					}
				}
				break;
				
			case '.':
				if(tape.containsKey(index))
					bih.output(tape.get(index));
				else
					bih.output(0);
				break;
				
			case ',':
				tape.put(index,bih.getInput());
				break;
			}	
		}
		
		bih.endOfCode();
		
	}

}
