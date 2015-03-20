package chess;

public abstract class Piece {

	private Tile currTile;
	
	public Piece(Tile currTile){
		this.currTile = currTile;
	}
	
	public Tile getCurrTile(){
		return currTile;
	}
	
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
	
	public abstract void drawLabel(Tile drawTo);
	
	public abstract Tile[] getValidMoves();
}
