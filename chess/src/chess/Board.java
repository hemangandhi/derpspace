package chess;

public class Board {

	private Tile[][] grid;
	private Tile selected;
	
	public Board(){
		grid = new Tile[8][8];
		
		for(int i = 0; i < 8; i++){
			for(int j = 0; j < 8; j++){
				grid[i][j] = new Tile(i,j,this);
			}
		}
		
		selected = null;
	}
	
	public Tile getSelectedTile(){
		return selected;
	}
	
	public void updateSelection(Tile newSelection){
		selected = newSelection;
	}
	
	public void restoreAllBackgrounds(){
		for(Tile[] row: grid){
			for(Tile t: row)
				t.restoreBackgroundColor();
		}
	}
	
	public Tile[][] getGrid(){
		return grid;
	}
	
	public boolean isFirstSelectionMade(){
		return selected != null;
	}
}
