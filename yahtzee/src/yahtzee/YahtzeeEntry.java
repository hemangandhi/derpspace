package yahtzee;

import java.util.HashSet;

/**
 * Handles all the scoring of a dice roll and 
 * everything about each yahtzee play (expect forced joker).
 * @author Heman
 *
 */
public enum YahtzeeEntry {
	
	ONES("Ones"),TWOS("Twos"),THREES("Threes"),FOURS("Fours"),FIVES("Fives"),SIXES("Sixes"),
	THREE_OF_KIND("3 of a kind"),FOUR_OF_KIND("4 of a kind"),FULL_HOUSE("Full House"),
	SHORT_STRAIGHT("Short Straight"),LONG_STRAIGHT("Long Straight"),
	YAHTZEE("Yahtzee"),CHANCE("Chance");

	private String label;
	
	private YahtzeeEntry(String label){
		this.label = label;
	}
	
	public String toString(){
		return label;
	}
	
	/**
	 * Get the sum of the dice value (useful for scoring).
	 * @param dr the dice panel containing the current roll.
	 * @return the sum of all the dice values.
	 */
	public static int getDiceSum(DicePanel dr){
		int [] roll = dr.getDice();
		int sum = 0;
		for(int i: roll)
			sum += i;
		return sum;
	}
	
	/**
	 * Get the sum of the dice values that match a certain value.
	 * @param dr the dice panel containing the roll of interest.
	 * @param toMatch the value to match.
	 * @return the sum of all the "toMatch" in the given dice roll. 
	 */
	public static int getDiceSum(DicePanel dr, int toMatch){
		int [] rolls = dr.getDice();
		int sum = 0;
		for(int i: rolls)
			if(i == toMatch)
				sum += i;
		return sum;
	}
	
	/**
	 * Gets the length of the longest match.
	 * @param dr the dice panel containing the roll.
	 * @return the length of the longest match (ie. a yahtzee would return 5 here).
	 */
	public static int getLongestMatch(DicePanel dr){
		int currMax = 0;
		for(int i = 1; i <= 6; i++){
			int currSt = getDiceSum(dr,i)/i;
			if(currSt > currMax)
				currMax = currSt;
		}
		return currMax;
	}
	
	/**
	 * Returns whether the roll is a full house.
	 * For the forced joker rule, handles yahtzees as a full house.
	 * @param dr the roll.
	 * @return whether the roll is a full house.
	 */
	public static boolean isFullHouse(DicePanel dr){
		int lm = getLongestMatch(dr);
		if(lm == 5)
			return true;
		
		HashSet<Integer> nums = new HashSet<Integer>();
		for(int i: dr.getDice())
			nums.add(i);
		return nums.size() == 2 && lm == 3;
	}
	
	/**
	 * Returns whether the roll is a straight of 4 (ie. 1,2,3,4) in any order.
	 * @param dr the roll.
	 * @return whether the roll is a straight of 4 (a short straight).
	 */
	public static boolean is4Straight(DicePanel dr){
		HashSet<Integer> nums = new HashSet<Integer>();
		for(int i: dr.getDice())
			nums.add(i);
		if(!nums.contains(3) || !nums.contains(4))
			return false;
		else if(nums.contains(2) && nums.contains(5))
			return true;
		else
			return (nums.contains(1) && nums.contains(2)) || (nums.contains(5) && nums.contains(6));
	}
	
	/**
	 * Returns whether the roll is a straight of 5 (ie. 1,2,3,4,5) in any order.
	 * @param dr the roll.
	 * @return whether the roll is a straight of 5 (a long straight).
	 */
	public static boolean is5Straight(DicePanel dr){
		HashSet<Integer> nums = new HashSet<Integer>();
		for(int i: dr.getDice())
			nums.add(i);
		if(!(nums.contains(2) && nums.contains(3) && nums.contains(4) && nums.contains(5)))
			return false;
		else
			return nums.contains(1) || nums.contains(6);
		
	}
	
	/**
	 * Get all the entries that pertain to the upper table.
	 * @return the entries in the upper table.
	 */
	public static YahtzeeEntry[] upperValues(){
		return new YahtzeeEntry[]{ONES,TWOS,THREES,FOURS,FIVES,SIXES};
	}
	
	/**
	 * All the entries in the lower table.
	 * @return all the entries in the lower table.
	 */
	public static YahtzeeEntry[] lowerValues(){
		return new YahtzeeEntry[]{THREE_OF_KIND,FOUR_OF_KIND,
				SHORT_STRAIGHT,LONG_STRAIGHT,
				FULL_HOUSE,CHANCE,YAHTZEE};
	}
	
	/**
	 * Gets the score of a said entry for a given dice roll.
	 * @param ye the entry of interest.
	 * @param dr the panel with the roll of interest.
	 * @return the score of a given entry for the given roll.
	 */
	public static int getVal(YahtzeeEntry ye, DicePanel dr){
		switch(ye){
		case ONES:
			return getDiceSum(dr,1);
		case TWOS:
			return getDiceSum(dr,2);
		case THREES:
			return getDiceSum(dr,3);
		case FOURS:
			return getDiceSum(dr,4);
		case FIVES:
			return getDiceSum(dr,5);
		case SIXES:
			return getDiceSum(dr,6);
		case THREE_OF_KIND:
			if(getLongestMatch(dr) < 3)
				return 0;
			else
				return getDiceSum(dr);
		case FOUR_OF_KIND:
			if(getLongestMatch(dr) < 4)
				return 0;
			else
				return getDiceSum(dr);
		case CHANCE:
			return getDiceSum(dr);
		case FULL_HOUSE:
			if(isFullHouse(dr))
				return 25;
			else
				return 0;
		case LONG_STRAIGHT:
			if(is5Straight(dr))
				return 40;
			else
				return 0;
		case SHORT_STRAIGHT:
			if(is4Straight(dr))
				return 30;
			else
				return 0;
		case YAHTZEE:
			if(getLongestMatch(dr) < 5)
				return 0;
			else
				return 50;
		default:
			return 0;
		}
	}
	
	/**
	 * Gets the yahtzee entry by its string representation.
	 * @param s the string representation.
	 * @return the yahtzee entry with that string representation (or null).
	 */
	public static YahtzeeEntry getByString(String s){
		for(YahtzeeEntry ye: values())
			if(ye.toString().equals(s))
				return ye;
		return null;
	}
}
