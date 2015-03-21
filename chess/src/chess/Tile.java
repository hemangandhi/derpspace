package chess;

import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;

public class Tile extends JLabel implements MouseListener{

	private int x, y;
	private Piece occupant;
	private Board environs;
	
	/**
	 * Create a basic tile.
	 * @param x the x position.
	 * @param y the y position.
	 * @param surroundings the board the tile is part of.
	 */
	public Tile(int x, int y, Board surroundings){
		this.x = x;
		this.y = y;
		environs = surroundings;
		occupant = null;
		
		addMouseListener(this);
		setOpaque(true);
		restoreBackgroundColor();
		
		setBorder(new LineBorder(Color.BLACK));
	}
	
	/**
	 * Gets the occupant of the tile.
	 * @return The current occupant or null if there is none.
	 */
	public Piece getOccupant(){
		return occupant;
	}
	
	/**
	 * Sets the occupant, re-drawing as needed.
	 * @param p the new occupant.
	 */
	public void setOccupant(Piece p){
		occupant = p;
		if(p != null)
			p.drawLabel(this);
		else
			setText("");
	}
	
	/**
	 * Restores the background for a checker-board configuration.
	 */
	public void restoreBackgroundColor(){
		if(x % 2 == y % 2)
			setBackground(Color.BLACK);
		else
			setBackground(Color.WHITE);
	}
	
	/**
	 * Get the surrounding board.
	 * @return the surrounding board.
	 */
	public Board getBoard(){
		return environs;
	}

	@Override
	public void mouseClicked(MouseEvent arg0) {
		if(environs.isFirstSelectionMade()){
			boolean moved = environs.getSelectedTile().getOccupant().move(this);
			environs.restoreAllBackgrounds();
			environs.updateSelection(null,moved);
		}else if(occupant != null){
			Tile[] t = occupant.getValidMoves();
			if(t != null)
			    for(Tile a: t)
				    if(a != null)
					    a.setBackground(Color.BLUE);
			environs.updateSelection(this,false);
		}
	}
	
	/**
	 * Get the x position of the tile
	 * @return the x position of the tile. 
	 */
	public int getX(){
		return x;
	}
	
	/**
	 * Get the y position of the tile.
	 * @return the y position of the tile.
	 */
	public int getY(){
		return y;
	}

	@Override
	public void mouseEntered(MouseEvent e) {
		setBorder(new LineBorder(Color.RED));
	}

	@Override
	public void mouseExited(MouseEvent e) {
		setBorder(new LineBorder(Color.BLACK));
		
	}

	@Override
	public void mousePressed(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}
}
