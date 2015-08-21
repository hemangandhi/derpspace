package chess;

public class Bishop extends Piece {

	private int player;
	
	public Bishop(Tile currTile, int player) {
		super(currTile);
		this.player = player;
	}

	@Override
	public void drawLabel(Tile drawTo) {
		drawTo.setForeground(java.awt.Color.RED);
		if(player==0)
			drawTo.setText("White Bishop");
		else
			drawTo.setText("Black Bishop");
	}

	@Override
	public Tile[] getValidMoves() {
		if(getCurrTile().getBoard().getCurrPlayer() != player)
			return null;
		Tile [] toRet = new Tile[16];
		int index = 0;
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
