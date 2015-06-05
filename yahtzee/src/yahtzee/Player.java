package yahtzee;

import java.util.*;

public class Player {
	
	private HashMap<YahtzeeEntry,Integer> scores;
	private int timesYahtzeed;
	
	public Player(){
		timesYahtzeed = 0;
		scores = new HashMap<YahtzeeEntry,Integer>(0);
		for(YahtzeeEntry ye: YahtzeeEntry.values()){
			scores.put(ye, -1);
		}
	}
	
	public boolean isUsed(YahtzeeEntry ye){
		return scores.get(ye) != -1;
	}

	
	public int getScore(YahtzeeEntry ye){
		return scores.get(ye);
	}
	
	public void setScore(YahtzeeEntry ye, int total){
		if(ye == YahtzeeEntry.YAHTZEE){
			if(timesYahtzeed > 0)
				scores.put(ye,total + scores.get(ye));
			else
				scores.put(ye, total);
			timesYahtzeed++;
		}else	
			scores.put(ye,total);
	}
	
	public int timesYahtzeed(){
		return timesYahtzeed;
	}
	
	public int getTotalScore(){
		return getLowerScore() + getUpperScore();
	}
	
	public int getLowerScore(){
		int sum = 0;
		for(YahtzeeEntry ye: YahtzeeEntry.lowerValues())
			if(scores.get(ye) != -1)
				sum += scores.get(ye);
		return sum;
	}
	
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
