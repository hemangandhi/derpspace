package othello_1;

import java.awt.Color;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.JLabel;
import javax.swing.border.*;

public class Tile extends JLabel implements MouseListener {

	private UIHandler<Tile> gui;
	private int x,y;
	
	public Tile(Color background, UIHandler<Tile> ui, int x, int y){
		super();
		gui = ui;
		this.x = x;
		this.y = y;
		setBackground(background);
		setOpaque(true);
		addMouseListener(this);
		setBorder(new LineBorder(Color.BLACK));
	}
	
	@Override
	public void mouseClicked(MouseEvent arg0) {
		// TODO Auto-generated method stub
		if(isClickable()){
			setBackground(gui.getTurn());
			flipFlips();
			gui.updateTurn(0);
		}
	}
	
	private void flipFlips(){
		for(Tile t: getFlips(true))
			t.setBackground(getBackground());
	}
	
	public boolean isClickable(){
		return getBackground().equals(gui.getDefaultColor()) && getFlips(false).length > 0;
	}
	
	private Tile[] getFlips(boolean played){
		Tile [][] board = gui.getGrid();
		Tile [] toRet = new Tile[board.length * board.length];
		int addIndex = 0;
		
		for(int i = -1; i <= 1; i++){
			for(int j = -1; j <= 1; j++){
				if(i == 0 && j == 0)
					continue;
				
				int n = 1;
				boolean breakCase = false;
				
				while(n*i + x >= 0 && n*i + x < board.length
						&& n*j + y >= 0 && n*j + y < board[0].length
						&& !breakCase){
					if(board[n*i + x][n*j + y].getBackground().equals(gui.getTurn()))
						breakCase = true;
					else if(board[n*i + x][n*j + y].getBackground().equals(gui.getDefaultColor())){
						n = 1;
						breakCase = true;
					}else	
						n++;
				}
				
				if(!breakCase)
					continue;
				
				n--;
				while(n > 0){
					toRet[addIndex] = board[n*i + x][n*j + y];
					addIndex++; n--;
				}
			}
		}
		
		int i;
		for(i = 0; i < toRet.length; i++){
			if(toRet[i] == null)
				break;
		}
		
		Tile[] returned = new Tile[i];
		for(int j = 0; j < returned.length; j++)
			returned[j] = toRet[j];
		return returned;
	}

	@Override
	public void mouseEntered(MouseEvent e) {
		if(isClickable())
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
