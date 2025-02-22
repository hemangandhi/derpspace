package chess;

public class Knight extends Piece {

	private int player;
	
	public Knight(Tile currTile, int player) {
		super(currTile);
		this.player = player;
	}

	@Override
	public void drawLabel(Tile drawTo) {
		drawTo.setForeground(java.awt.Color.RED);
		if(player == 0)
			drawTo.setText("White knight");
		else
			drawTo.setText("Black knight");
	}

	@Override
	public Tile[] getValidMoves() {
		if(getCurrTile().getBoard().getCurrPlayer() != player)
			return null;
		
		Tile[] toRet = new Tile[8];
		int y = getCurrTile().getX();
		int x = getCurrTile().getY();
		Tile [][] grid = getCurrTile().getBoard().getGrid();
		if(y + 2 < 8){
			if(x + 1 < 8)
				toRet[0] = grid[x + 1][y + 2];
			if(x - 1 >= 0)
				toRet[1] = grid[x - 1][y + 2];
		}
		if(y + 1 < 8){
			if(x + 2 < 8)
				toRet[2] = grid[x + 2][y + 1];
			if(x - 2 >= 0)
				toRet[3] = grid[x - 2][y + 1];
		}
		if(y - 1 >= 0){
			if(x + 2 < 8)
				toRet[4] = grid[x + 2][y - 1];
			if(x - 2 >= 0)
				toRet[5] = grid[x - 2][y - 1];
		}
		if(y - 2 >= 0){
			if(x + 1 < 8)
				toRet[6] = grid[x + 1][y - 2];
			if(x - 1 >= 0)
				toRet[7] = grid[x - 1][y - 2];
		}
		return toRet;
	}

}
