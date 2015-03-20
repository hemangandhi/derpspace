package chess;

public class King extends Piece {
	
	private int player;

	public King(Tile currTile, int player) {
		super(currTile);
		this.player = player;
	}

	@Override
	public void drawLabel(Tile drawTo) {
		drawTo.setForeground(java.awt.Color.RED);
		if(player == 0)
			drawTo.setText("White King");
		else
			drawTo.setText("Black King");
	}

	@Override
	public Tile[] getValidMoves() {
		if(getCurrTile().getBoard().getCurrPlayer() != player)
			return null;
		Tile [] toRet = new Tile[8];
		int index = 0;
		for(int i = Math.max(0,getCurrTile().getY() - 1); i <= getCurrTile().getY() + 1 && i < 8; i++){
			for(int j = Math.max(0,getCurrTile().getX() - 1); j <= getCurrTile().getX() + 1 && j < 8; j++){
				if( i != getCurrTile().getY() || j != getCurrTile().getX()){
					toRet[index] = getCurrTile().getBoard().getGrid()[i][j];
					index++;
				}
			}
		}
		return toRet;
	}

}
