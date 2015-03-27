package chess;

public abstract class Piece {

	private Tile currTile;
	
	public Piece(Tile currTile){
		this.currTile = currTile;
	}
	
	public Tile getCurrTile(){
		return currTile;
	}
	
	/**
	 * Tries to move the piece to tile dest.
	 * Fails if the move is invalid.
	 * Updates all tiles as necessary (removing previous occupants of dest, if any).
	 * @param dest the destination tile.
	 * @return whether or not the tile was successfully moved.
	 */
	public boolean move(Tile dest){
		Tile [] valids = getValidMoves();
		if(valids != null){
			for(Tile t: getValidMoves()){
				if(t == dest){
					currTile.setOccupant(null);
					currTile = dest;
					currTile.setOccupant(this);
					return true;
				}
			}
		}
		return false;
	}
	
	public void drawLabel(){
		drawLabel(getCurrTile());
	}
	
	/**
	 * Draws the occupied tile.
	 * @param drawTo the tile to draw to.
	 */
	public abstract void drawLabel(Tile drawTo);
	
	/**
	 * Returns an array of the valid moves. These may contain nulls.
	 * @return an array of the valid moves.
	 */
	public abstract Tile[] getValidMoves();
}
