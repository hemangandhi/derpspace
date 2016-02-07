package general;

import java.util.HashSet;

public class MatUtils {
	
	public static boolean inRange(Object[][] m, int x , int y){
		return 0 <= x && x < m.length && 0 <= y && y < m[x].length;
	}

	public static <T> boolean noDupsInRows(T[][] arg){
		for(int i = 0; i < arg.length; i++){
			HashSet<T> hs = new HashSet<T>();
			for(int j = 0; j < arg[i].length; j++){
				hs.add(arg[i][j]);
			}
			
			if(hs.size() < arg[i].length)
				return false;
		}
		
		return true;
	}
	
	public static <T> boolean noDupsInCols(T[][] arg){
		for(int i = 0; i < arg[0].length; i++){
			HashSet<T> hs = new HashSet<T>();
			for(int j = 0; j < arg.length; j++){
				hs.add(arg[j][i]);
			}
			
			if(hs.size() < arg.length)
				return false;
		}
		
		return true;
	}
	
	public static<T> boolean notNAdj(T[][] mat, int adj, boolean diag){
		int [][] nbors;
		if(diag){
			nbors = new int[8][2];
			for(int i = -1, c = 0; i <= 1; i++){
				for(int j = -1; j <= 1; j++){
					if(i == j && j == 0)
						continue;
					
					nbors[c] = new int[]{i , j};
					c++;
				}
			}
		}else{
			nbors = new int[][]{{1, 0}, {0, 1}, {-1 , 0}, {0, -1}};
		}
		
		for(int i = 0; i < mat.length; i++){
			for(int j = 0; j < mat[i].length; j++){
				for(int[] dir: nbors){
					int nc = 0;
					for(int x = i + dir[0], y = j + dir[1];
							inRange(mat, x, y) && mat[x][y].equals(mat[i][j]);
							nc++, x = i + (nc + 1)*dir[0], y = j + (nc + 1)*dir[1]);
					if(nc >= adj)
						return false;
				}
			}
		}
		
		return true;
	}
	
	public static <T> boolean noRowMatch(T[][] mat){
		for(int i = 0; i < mat.length; i++){
			for(int j = i + 1; j < mat.length; j++){
				boolean allEq = true;
				for(int k = 0; k < mat[0].length; k++){
					if(!mat[i][k].equals(mat[j][k]))
						allEq = false;
				}
				if(allEq)
					return false;
			}
		}
		
		return true;
	}
	
	public static <T> boolean noColMatch(T[][] mat){
		for(int i = 0; i < mat[0].length; i++){
			for(int j = i + 1; j < mat[0].length; j++){
				boolean allEq = true;
				for(int k = 0; k < mat.length; k++){
					if(!mat[k][i].equals(mat[k][j]))
						allEq = false;
				}
				if(allEq)
					return false;
			}
		}
		
		return true;
	}
	
	public static <T> int countInRow(T[][] mat, T val, int row){
		int c = 0;
		for(int i = 0; i < mat[row].length; i++)
			if(mat[row][i].equals(val))
				c++;
				
		return c;		
	}
	
	public static <T> int countInCol(T[][] mat, T val, int col){
		int c = 0;
		
		for(int i = 0; i < mat.length; i++)
			if(mat[i][col].equals(val))
				c++;
		
		return c;
	}
}
