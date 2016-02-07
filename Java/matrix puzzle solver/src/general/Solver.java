package general;

import java.util.Set;

public class Solver {

	public static <T> T[][] solve(T[][] known, Validator<T> val){
		int rows = known.length;
		int cols = known[0].length;
		
		Set<T>[][] board =  (Set<T>[][])new Set[rows][cols];
		for(int i = 0; i < rows; i++)
			for(int j = 0; j < cols; j++)
				if(known[i][j] == null)
					board[i][j] = val.getStates();
				else
					board[i][j] = null;
		
		Set<T>[][] filtered = (Set<T>[][])new Set[rows][cols];
		for(int i = 0; i < rows; i++)
			for(int j = 0; j < cols; j++){
				if(board[i][j] == null){
					filtered[i][j] = null;
					continue;
				}
				filtered[i][j] = val.filter(board[i][j], i, j, (T[][])known);
				if(filtered[i][j].size() == 0)
					throw new IllegalStateException("No solution!");
			}
		
		T[][] nKnown = (T[][]) new Object[rows][cols];
		boolean recurse = false;
		for(int i = 0; i < rows; i++){
			for(int j = 0; j < cols; j++){
				if(filtered[i][j] != null && filtered[i][j].size() == 1)
					nKnown[i][j] = (T) filtered[i][j].toArray()[0];
				else if(filtered[i][j] != null){
					recurse = true;
				}else{
					nKnown[i][j] = known[i][j];
				}
			}
		}
		
		if(recurse){
			int ml = filtered[0][0].size();
			int mr = 0, mc = 0;
			for(int i = 0; i < rows; i++){
				for(int j = 0; j < cols; j++){
					if(filtered[i][j].size() < ml){
						ml = filtered[i][j].size();
						mr = i;
						mc = j;
					}
				}
			}
			
			while(!filtered[mr][mc].isEmpty()){
				try{
					nKnown[mr][mc] = (T) filtered[mr][mc].toArray()[0];
					filtered[mr][mc].remove(nKnown[mr][mc]);
					if(!val.validate(nKnown)) 
						continue;
					
					return solve(nKnown, val);
				}catch(IllegalStateException e){
				
				}
			}
			
			throw new IllegalStateException("No Solution");
		}else{
			return nKnown;
		}
	}
}
