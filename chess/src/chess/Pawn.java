package chess;

public class Pawn extends Piece {

	private int player;
	
	public Pawn(Tile currTile, int player) {
		super(currTile);
		
		this.player = player;
	}

	@Override
	public void drawLabel(Tile drawTo) {
		drawTo.setForeground(java.awt.Color.RED);
		if(player == 0)	
			drawTo.setText("White pawn");
		else
			drawTo.setText("Black pawn");

	}

	@Override
	public Tile[] getValidMoves() {
		Tile curr = getCurrTile();
		int moveY = (player == 0)? curr.getY() + 1: curr.getY() - 1;
		if(curr.getBoard().getCurrPlayer() != player || moveY >= 8 || moveY < 0)
			return null;
		else{
			Tile [] toRet = new Tile[4];
			if(curr.getBoard().getGrid()[moveY][curr.getX()].getOccupant() == null)
				toRet[0] = curr.getBoard().getGrid()[moveY][curr.getX()];
			if(curr.getX() - 1 >= 0 && 
					curr.getBoard().getGrid()[moveY][curr.getX() - 1].getOccupant() != null)
				toRet[1] = curr.getBoard().getGrid()[moveY][curr.getX() - 1];
			if(curr.getX() + 1 < 8 &&
					curr.getBoard().getGrid()[moveY][curr.getX() + 1].getOccupant() != null)
				toRet[2] = curr.getBoard().getGrid()[moveY][curr.getX() + 1];
			if(((player == 0 && curr.getY() == 1) || (player == 1 && curr.getY() == 6))
					&& toRet[0] != null &&
					curr.getBoard().getGrid()[curr.getY() + 2 * ((player == 0)? 1 : -1)][curr.getX()].getOccupant() == null)
				toRet[3] = curr.getBoard().getGrid()[curr.getY() + 2 * ((player == 0)? 1 : -1)][curr.getX()];
			return toRet;
		}
	}

}
