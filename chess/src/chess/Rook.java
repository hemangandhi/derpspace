package chess;

public class Rook extends Piece {

	private int player;
	
	public Rook(Tile currTile, int player) {
		super(currTile);
		this.player = player;
	}

	@Override
	public void drawLabel(Tile drawTo) {
		drawTo.setForeground(java.awt.Color.RED);
		if(player == 0)
			drawTo.setText("White rook");
		else
			drawTo.setText("Black rook");
	}

	@Override
	public Tile[] getValidMoves() {
		if(getCurrTile().getBoard().getCurrPlayer() != player)
			return null;
		Tile [] toRet = new Tile[16];
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
		return toRet;
	}

}
