package chess;

import java.awt.Graphics;
import java.awt.GridLayout;

import javax.swing.*;

public class Board extends JPanel{

	private Tile[][] grid;
	private Tile selected;
	
	private int currPlayer;
	private int totalPlayers;
	
	/**
	 * Creates a generic board with numPlayers total players.
	 * @param numPlayers the number of players in the game.
	 */
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
	
	/**
	 * Returns the user selected tile, null if no selection is made.
	 * @return the user selected tile or null.
	 */
	public Tile getSelectedTile(){
		return selected;
	}
	
	/**
	 * Updates the selected tile.
	 * @param newSelection the newly selected tile.
	 * @param turn whether the new selection completed a turn.
	 */
	public void updateSelection(Tile newSelection, boolean turn){
		selected = newSelection;
		if(newSelection == null && turn){
			currPlayer++;
			currPlayer %= totalPlayers;
		}
	}
	
	/**
	 * Returns the current player (an integer x where 0 <= x < total players).
	 * @return the current player.
	 */
	public int getCurrPlayer(){
		return currPlayer;
	}
	
	public void paintComponent(Graphics g){
		restoreAllBackgrounds();
	}
	
	/**
	 * Restores the grid to a checker-board pattern.
	 */
	public void restoreAllBackgrounds(){
		for(Tile[] row: grid){
			for(Tile t: row)
				t.restoreBackgroundColor();
		}
	}
	
	/**
	 * Returns the grid.
	 * @return the grid.
	 */
	public Tile[][] getGrid(){
		return grid;
	}
	
	/**
	 * Returns whether or not the user has made their first selection.
	 * @return whether or not the user has made their first selection.
	 */
	public boolean isFirstSelectionMade(){
		return selected != null;
	}
	
	/**
	 * Creates a chess board set up.
	 * @return the set up chess board.
	 */
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
		
		//Add bishops
		board[0][2].setOccupant(new Bishop(board[0][2],0));
		board[0][5].setOccupant(new Bishop(board[0][5],0));
		board[7][2].setOccupant(new Bishop(board[7][2],1));
		board[7][5].setOccupant(new Bishop(board[7][5],1));
		
		//Add queens
		board[0][3].setOccupant(new Queen(board[0][3],0));
		board[7][3].setOccupant(new Queen(board[7][3],1));
		
		toRet.repaint();
		return toRet;
	}
	
	/**
	 * Draws a coordinate grid (for testing purposes).
	 * @return the generated coordinate grid.
	 */
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
