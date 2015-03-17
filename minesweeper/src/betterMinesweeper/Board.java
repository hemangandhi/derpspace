package betterMinesweeper;

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;

public class Board extends JFrame {

	private Tile[][] tiles;
	private JCheckBox flagging;
	private int opens, minesAdded;
	private int numFlags;
	private boolean gameOver;
	
	public Board(int height, int width, int numMines) {
		super("Minesweeper");
		tiles = new Tile[width][height];
		opens = 0;
		setUpBoard(numMines);
	}
	
	private void setUpBoard(int numMines){
		setSize(1500,1000);
		setVisible(true);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setLayout(new BorderLayout());
		
		JPanel top = new JPanel();
		top.setLayout(new GridLayout(tiles.length, tiles[0].length));
		
		minesAdded = 0;
		for(int i = 0; i < tiles.length; i++){
			for(int j = 0; j < tiles[i].length; j++){
				
				boolean mine;
				if(minesAdded >= numMines)
					mine = false;
				else if(tiles.length*tiles[0].length - (i+1)*(j+1) <= numMines - minesAdded)
					mine = true;
				else
					mine = Math.random()*100 < numMines*100/(tiles.length*tiles[0].length);
				if(mine)
					minesAdded++;
				Tile toAdd = new Tile(mine,this);
				
				for(int ia = -1; ia < 2; ia++){
					for(int ja = -1; ja < 2; ja++){
						if(i + ia >= 0 && i + ia < tiles.length && !(ia == 0 && ja == 0)){
							if(j + ja >= 0 && j + ja < tiles[i].length &&
									tiles[i + ia][j + ja] != null){
								tiles[i + ia][j + ja].addNeighbour(toAdd);
								toAdd.addNeighbour(tiles[i + ia][j + ja]);
							}
						}
					}
				}
				
				tiles[i][j] = toAdd;
				top.add(toAdd);
			}
		}
		
		top.setBackground(Color.WHITE);
		add(top);
		
		flagging = new JCheckBox("Select to flag mines!");
		add(flagging, BorderLayout.SOUTH);
	}
	
	public void handleLoss(){
		gameOver = true;
		
		for(int i = 0; i < tiles.length; i++){
			for(int j = 0; j < tiles[i].length; j++){
				tiles[i][j].open();
			}
		}
		
		boolean toContinue = JOptionPane.showConfirmDialog(this, "You have lost. Press OK to retry!", "Game over",
				JOptionPane.OK_CANCEL_OPTION,
				JOptionPane.INFORMATION_MESSAGE) == JOptionPane.OK_OPTION;
		
		if(toContinue)
			GameRunner.runGame();
		
		this.dispose();
	}
	
	public void handleWin(){
		gameOver = true;
		
		boolean toContinue = JOptionPane.showConfirmDialog(this, "You have won! Press OK to retry!", "Game over",
				JOptionPane.OK_CANCEL_OPTION,
				JOptionPane.INFORMATION_MESSAGE) == JOptionPane.OK_OPTION;
		
		if(toContinue)
			GameRunner.runGame();
		
		this.dispose();
	}
	
	public boolean flagging(){
		return flagging.isSelected();
	}
	
	public boolean gameOver(){
		return gameOver;
	}
	
	public void addClick(boolean add, boolean flag){
		if(add){
			opens++;
			if(flag)
				numFlags++;
			if(opens == tiles.length*tiles[0].length && numFlags == minesAdded)
				handleWin();
		}else{
			if(flag)
				numFlags--;
			else
				opens--;
		}
			
	}

}
