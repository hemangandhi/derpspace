package betterMinesweeper;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.*;
import javax.swing.JLabel;
import javax.swing.border.*;

public class Tile extends JLabel implements MouseListener {


	private Tile[] neighbours;
	private int numMines;
	private boolean isMine;
	private byte state;
	private Board game;
	
	public Tile(boolean isMine, Board game){
		super("Open");
		this.isMine = isMine;
		state = 0;
		neighbours = new Tile[8];
		numMines = 0;
		setOpaque(true);
		setBackground(Color.WHITE);
		this.game = game;
		addMouseListener(this);
		setBorder(new LineBorder(Color.BLACK));
	}
	
	public void addNeighbour(Tile t){
		for(int i = 0; i < neighbours.length; i++){
			if(neighbours[i] == null){
				neighbours[i] = t;
				break;
			}
		}
		
		if(t.isMine())
			numMines++;
	}
	
	public boolean isMine(){
		return isMine;
	}
	
	public void open(){
		if(state == 0){
			state++;
			
			if(isMine){
				if(!game.gameOver())
					game.handleLoss();
			}else
				game.addClick(true, false);
			
			if(numMines == 0){
				openNeighbours();
			}
			
			setText((isMine)?"*":numMines + "");
			setToolTipText(getText());
			setBackground((isMine)?Color.GRAY:
				new Color(0, (int) (255 - 255*numMines/8.0), 0));
		}
	}
	
	public boolean isOpen(){
		return state == 1;
	}
	
	public void openNeighbours(){
		for(Tile t: neighbours){
			if(t != null && !t.isOpen())
				t.open();
		}
	}
	
	public void toggleFlag(){
		if(state == 0){
			state--;
			setText("Flagged!");
			setBackground(Color.RED);
			game.addClick(true, true);
		}else if(state == -1){
			state++;
			setText("open");
			setBackground(Color.WHITE);
			game.addClick(false, true);
		}
	}
	
	public boolean isFlagged(){
		return state == -1;
	}
	
	@Override
	public void mouseClicked(MouseEvent arg0) {
		if(state != 1){
			if(game.flagging())
				toggleFlag();
			else
				open();
		}else{
			for(Tile t: neighbours)
				if(t != null && !t.isFlagged())
					t.open();
		}
	}

	@Override
	public void mouseEntered(MouseEvent e) {
		if(game.flagging() && state != 1)
			setBorder(new LineBorder(Color.RED));
		else if(state == 0)
			setBorder(new LineBorder(Color.BLUE));
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
