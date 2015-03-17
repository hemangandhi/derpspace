package chess;

import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;

public class Tile extends JLabel implements MouseListener{

	private int x, y;
	private Piece occupant;
	private Board environs;
	
	public Tile(int x, int y, Board surroundings){
		this.x = x;
		this.y = y;
		environs = surroundings;
		
		addMouseListener(this);
		setOpaque(true);
		restoreBackgroundColor();
		
		setBorder(new LineBorder(Color.BLACK));
	}
	
	public Piece getOccupant(){
		return occupant;
	}
	
	public void setOccupant(Piece p){
		occupant = p;
		if(p != null)
			p.drawLabel();
	}
	
	public void restoreBackgroundColor(){
		if(x % 2 == y % 2)
			setBackground(Color.BLACK);
		else
			setBackground(Color.WHITE);
	}

	@Override
	public void mouseClicked(MouseEvent arg0) {
		if(environs.isFirstSelectionMade()){
			boolean moved = environs.getSelectedTile().getOccupant().move(this);
			if(moved){
				environs.restoreAllBackgrounds();
				environs.updateSelection(null);
			}
		}
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
