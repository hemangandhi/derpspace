package yahtzee;

import java.util.*;

public class Player {
	
	private HashMap<YahtzeeEntry,Integer> scores;
	private int timesYahtzeed;
	
	/**
	 * Starts a player who has played nothing.
	 */
	public Player(){
		timesYahtzeed = 0;
		scores = new HashMap<YahtzeeEntry,Integer>(0);
		for(YahtzeeEntry ye: YahtzeeEntry.values()){
			scores.put(ye, -1);
		}
	}
	
	/**
	 * Returns whether the player has played on a given entry.
	 * @param ye the entry.
	 * @return whether the player's played on the given entry.
	 */
	public boolean isUsed(YahtzeeEntry ye){
		return scores.get(ye) != -1;
	}

	/**
	 * Gets the player score for an entry.
	 * Un-used entries are 0.
	 * @param ye the entry of interest.
	 * @return the score the player got for the entry.
	 */
	public int getScore(YahtzeeEntry ye){
		int toRet = scores.get(ye);
		if(toRet == -1)
			return 0;
		else
			return toRet;
	}
	
	/**
	 * Sets the score for a given entry.
	 * Handles oddities with scoring yahtzees, such as:
	 * - the second time adds with the first.
	 * - a 0 alters the forced joker rule.
	 * @param ye the entry.
	 * @param total the score to set to.
	 */
	public void setScore(YahtzeeEntry ye, int total){
		if(ye == YahtzeeEntry.YAHTZEE){
			if(total == 0){
				timesYahtzeed = -1;
				scores.put(ye,total);
				return;
			}	
			if(timesYahtzeed > 0)
				scores.put(ye,total + scores.get(ye));
			else
				scores.put(ye, total);
			timesYahtzeed++;
		}else	
			scores.put(ye,total);
	}
	
	/**
	 * Returns the number of times the player has gotten a yahtzee.
	 * @return the number of times the player has gotten a yahtzee.
	 */
	public int timesYahtzeed(){
		return timesYahtzeed;
	}
	
	/**
	 * Gets the player's total score.
	 * @return the player's total score.
	 */
	public int getTotalScore(){
		return getLowerScore() + getUpperScore();
	}
	
	/**
	 * Gets the player score for the lower table.
	 * @return the player score for the lower table.
	 */
	public int getLowerScore(){
		int sum = 0;
		for(YahtzeeEntry ye: YahtzeeEntry.lowerValues())
			if(scores.get(ye) != -1)
				sum += scores.get(ye);
		return sum;
	}
	
	/**
	 * Gets the upper table score (handling the 35 point bonus).
	 * @return the upper table score.
	 */
	public int getUpperScore(){
		int sum = 0;
		for(YahtzeeEntry ye: YahtzeeEntry.upperValues()){
			if(scores.get(ye) != -1)
				sum += scores.get(ye);
		}
		if(sum >= 63)
			return sum + 35;
		else
			return sum;
	}
}
