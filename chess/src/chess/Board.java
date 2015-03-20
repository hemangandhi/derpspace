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
		//Get variables
		Board toRet = new Board(2);
		Tile[][] board = toRet.getGrid();
		
		//Add pawns
		for(int i = 0; i < 8; i++){
			board[1][i].setOccupant(new Pawn(board[1][i],0));
			board[6][i].setOccupant(new Pawn(board[6][i],1));
		}
		
		//Add knights
		board[0][1].setOccupant(new Knight(board[0][1],0));
		board[0][6].setOccupant(new Knight(board[0][6],0));
		board[7][1].setOccupant(new Knight(board[7][1],1));
		board[7][6].setOccupant(new Knight(board[7][6],1));
		
		//Add kings
		board[0][4].setOccupant(new King(board[0][4],0));
		board[7][4].setOccupant(new King(board[7][4],1));
		
		//Add rooks
		board[0][0].setOccupant(new Rook(board[0][0],0));
		board[0][7].setOccupant(new Rook(board[0][7],0));
		board[7][0].setOccupant(new Rook(board[7][0],1));
		board[7][7].setOccupant(new Rook(board[7][7],1));
		
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
