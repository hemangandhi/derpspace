
public class MazeGrid {

	private MazeNode[][] maze;
	
	public MazeGrid(int rows, int cols){
		maze = new MazeNode[rows][cols];
		
		for(int i = 0; i < rows; i++){
			for(int j = 0; j < cols; j++)
				maze[i][j] = new MazeNode(i,j);
		}
		
		for(int i = 0; i < rows; i++){
			for(int j = 0; j < cols; j++){
				if(i + 1 < rows)
					maze[i][j].addNieghbour(maze[i + 1][j]);
				if(i - 1 > 0)
					maze[i][j].addNieghbour(maze[i - 1][j]);
				if(j + 1 < cols)
					maze[i][j].addNieghbour(maze[i][j + 1]);
				if(j - 1 > 0)
					maze[i][j].addNieghbour(maze[i][j - 1]);
			}
		}
		genMaze();
	}
	
	public MazeNode[][] getMaze(){
		return maze;
	}
	
	public void genMaze(){
		for(int i = 1; i < maze.length; i++)
			for(int j = 0; j < maze[i].length; j++)
				genPath(0,0,i,j);
	}
	
	public void genPath(int srcRow, int srcCol, int destRow, int destCol){
		if(srcRow == destRow && srcCol == destCol)
			return;
		else if(maze[srcRow][srcCol].isConnected(maze[destRow][destCol]))
			return;
		MazeNode toConnect;
		try{
			toConnect = maze[srcRow][srcCol].getRandomDisconnectedNeighbour();
		}catch(Exception e){
			toConnect = maze[srcRow][srcCol].getRandomConnection();
		}
		maze[srcRow][srcCol].connect(toConnect);
		genPath(toConnect.getX(),toConnect.getY(),destRow,destCol);
	}
	
	
}
