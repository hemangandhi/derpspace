package chess;

public class Queen extends Piece {

	private int player;
	
	public Queen(Tile currTile, int player) {
		super(currTile);
		this.player = player;
	}

	@Override
	public void drawLabel(Tile drawTo) {
		drawTo.setForeground(java.awt.Color.RED);
		if(player == 0)
			drawTo.setText("White Queen");
		else
			drawTo.setText("Black Queen");
	}

	@Override
	public Tile[] getValidMoves() {
		if(getCurrTile().getBoard().getCurrPlayer() != player)
			return null;
		Tile [] toRet = new Tile[32];
		int index = 0;
		for(int y = getCurrTile().getY() - 1; y >= 0; y--){
			toRet[index] = getCurrTile().getBoard().getGrid()[y][getCurrTile().getX()];
			index++;
			if(toRet[index - 1].getOccupant() != null)
				break;
		}
		for(int y = getCurrTile().getY() + 1; y < 8; y++){
			toRet[index] = getCurrTile().getBoard().getGrid()[y][getCurrTile().getX()];
			index++;
			if(toRet[index - 1].getOccupant() != null)
				break;
		}
		for(int x = getCurrTile().getX() + 1; x < 8; x++){
			toRet[index] = getCurrTile().getBoard().getGrid()[getCurrTile().getY()][x];
			index++;
			if(toRet[index - 1].getOccupant() != null)
				break;
		}
		for(int x = getCurrTile().getX() - 1; x >= 0; x--){
			toRet[index] = getCurrTile().getBoard().getGrid()[getCurrTile().getY()][x];
			index++;
			if(toRet[index - 1].getOccupant() != null)
				break;
		}
		for(int i = 1; i + getCurrTile().getX() < 8 && i + getCurrTile().getY() < 8; i++){
			toRet[index] = getCurrTile().getBoard().getGrid()[i + getCurrTile().getY()]
					[i + getCurrTile().getX()];
			index++;
			if(toRet[index - 1].getOccupant() != null)
				break;
		}
		for(int i = 1; getCurrTile().getX() - i >= 0 && getCurrTile().getY() - i >= 0; i++){
			toRet[index] = getCurrTile().getBoard().getGrid()
					[getCurrTile().getY() - i][getCurrTile().getX() - i];
			index++;
			if(toRet[index - 1].getOccupant() != null)
				break;
		}
		for(int i = 1; getCurrTile().getX() - i >= 0 && getCurrTile().getY() + i < 8; i++){
			toRet[index] = getCurrTile().getBoard().getGrid()[getCurrTile().getY() + i]
					[getCurrTile().getX() - i];
			index++;
			if(toRet[index - 1].getOccupant() != null)
				break;
		}
		for(int i = 1; getCurrTile().getY() - i >= 0 && getCurrTile().getX() + i < 8; i++){
			toRet[index] = getCurrTile().getBoard().getGrid()[getCurrTile().getY() - i]
					[getCurrTile().getX() + i];
			index++;
			if(toRet[index - 1].getOccupant() != null)
				break;
		}
		return toRet;
	}

}
