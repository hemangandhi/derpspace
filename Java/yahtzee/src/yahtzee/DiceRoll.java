package yahtzee;

import java.util.*;

public class DiceRoll {

	public static final int NUM_DICE = 5;
	public static final int DICE_SIDES = 6;
	
	private int [] roll;
	
	/**
	 * Makes an empty roll - as if no dice have been rolled.
	 */
	public DiceRoll(){
		roll = new int[NUM_DICE];
		
	}
	
	/**
	 * Rolls all die except those in exclusions.
	 * @param exclusions the indexes of the dice not rolled.
	 */
	public void rollDice(Collection<Integer> exclusions){
		for(int i = 0; i < roll.length; i++)
			if(!exclusions.contains(i))
				roll[i] = (int)((Math.random()*DICE_SIDES) + 1);
			
	}
	
	/**
	 * Returns all the dice rolled as an int array, position indicating which die.
	 * @return an int array representing the roll.
	 */
	public int [] getDice(){
		return roll;
	}
}
