package yahtzee;

import java.util.HashSet;

public enum YahtzeeEntry {
	
	ONES("Ones"),TWOS("Twos"),THREES("Threes"),FOURS("Fours"),FIVES("Fives"),SIXES("Sixes"),
	THREE_OF_KIND("3 of a kind"),FOUR_OF_KIND("4 of a kind"),
	SHORT_STRAIGHT("Short Straight"),LONG_STRAIGHT("Long Straight"),
	FULL_HOUSE("Full House"),CHANCE("Chance"),YAHTZEE("Yahtzee");

	private String label;
	
	private YahtzeeEntry(String label){
		this.label = label;
	}
	
	public String toString(){
		return label;
	}
	
	public static int getDiceSum(DiceRoll dr){
		int [] roll = dr.getDice();
		int sum = 0;
		for(int i: roll)
			sum += i;
		return sum;
	}
	
	public static int getDiceSum(DiceRoll dr, int toMatch){
		int [] rolls = dr.getDice();
		int sum = 0;
		for(int i: rolls)
			if(i == toMatch)
				sum += i;
		return sum;
	}
	
	public static int getLongestMatch(DiceRoll dr){
		int currMax = 0;
		for(int i = 1; i <= 6; i++){
			int currSt = getDiceSum(dr,i)/i;
			if(currSt > currMax)
				currMax = currSt;
		}
		return currMax;
	}
	
	public static boolean isFullHouse(DiceRoll dr){
		HashSet<Integer> nums = new HashSet<Integer>();
		for(int i: dr.getDice())
			nums.add(i);
		return nums.size() == 2 && getLongestMatch(dr) == 3;
	}
	
	public static boolean is4Straight(DiceRoll dr){
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
	
	public static boolean is5Straight(DiceRoll dr){
		HashSet<Integer> nums = new HashSet<Integer>();
		for(int i: dr.getDice())
			nums.add(i);
		if(!(nums.contains(2) && nums.contains(3) && nums.contains(4) && nums.contains(5)))
			return false;
		else
			return nums.contains(1) || nums.contains(6);
		
	}
	
	public static int getVal(YahtzeeEntry ye, DiceRoll dr){
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
	
	public static YahtzeeEntry getByString(String s){
		for(YahtzeeEntry ye: values())
			if(ye.toString().equals(s))
				return ye;
		return null;
	}
}
