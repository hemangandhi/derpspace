package chess;

import java.awt.GridLayout;
import javax.swing.*;

public class Board extends JPanel{

	private Tile[][] grid;
	private Tile selected;
	
	private int currPlayer;
	private int totalPlayers;
	
	public Board(int numPlayers){
		setLayout(new GridLayout(8,8));
		
		grid = new Tile[8][8];
		
		for(int i = 0; i < 8; i++){
			for(int j = 0; j < 8; j++){
				grid[i][j] = new Tile(j,i,this);
				add(grid[i][j]);
			}
		}
		
		selected = null;
		
		totalPlayers = numPlayers;
		currPlayer = 0;
	}
	
	public Tile getSelectedTile(){
		return selected;
	}
	
	public void updateSelection(Tile newSelection){
		selected = newSelection;
		if(newSelection == null){
			currPlayer++;
			currPlayer %= totalPlayers;
		}
	}
	
	public int getCurrPlayer(){
		return currPlayer;
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
	
	public static Board chessBoard(){
		Board toRet = new Board(2);
		Tile[][] board = toRet.getGrid();
		
		for(int i = 0; i < 8; i++){
			board[1][i].setOccupant(new Pawn(board[1][i],0));
			board[6][i].setOccupant(new Pawn(board[6][i],1));
			
			board[1][i].getOccupant().drawLabel();
			board[6][i].getOccupant().drawLabel();
		}
		
		toRet.repaint();
		return toRet;
	}
	
	public static Board drawGrid(){
		Board toRet = new Board(1);
		
		for(Tile [] row: toRet.getGrid()){
			for(Tile t: row){
				t.setForeground(java.awt.Color.RED);
				t.setText("(" + t.getX() + ", " + t.getY() + " )");
			}
		}
		
		return toRet;
	}
}
