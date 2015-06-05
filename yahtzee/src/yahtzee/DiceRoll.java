package yahtzee;

import java.util.*;

public class DiceRoll {

	public static final int NUM_DICE = 5;
	public static final int DICE_SIDES = 6;
	
	private int [] roll;
	
	public DiceRoll(){
		roll = new int[NUM_DICE];
		
	}
	
	public void rollDice(Collection<Integer> exclusions){
		for(int i = 0; i < roll.length; i++)
			if(!exclusions.contains(i))
				roll[i] = (int)((Math.random()*DICE_SIDES) + 1);
			
	}
	
	public int [] getDice(){
		return roll;
	}
}
