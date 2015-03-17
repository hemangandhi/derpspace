package othello_1;

import java.awt.*;
import javax.swing.*;

public class Game extends JFrame implements UIHandler<Tile> {

	private Color[] turns;
	private int turnCount;
	private Tile[][] board;
	private String[] names;
	private JLabel turnText;
	
	public Game(Color[] turns, String[] names){
		super("Othello");
		this.names = names;
		this.turns = turns;
		turnCount = 0;
		
		setLayout(new BorderLayout());
		
		JPanel topHalf = new JPanel(new GridLayout(8,8));
		board = new Tile[8][8];
		for(int i = 0; i < board.length; i++){
			for(int j = 0; j < board[i].length; j++){
				board[i][j] = new Tile(Color.GREEN,this,i,j);
				topHalf.add(board[i][j]);
			}
		}
		add(topHalf);
		
		board[3][3].setBackground(turns[0]);
		board[4][3].setBackground(turns[1]);
		board[3][4].setBackground(turns[1]);
		board[4][4].setBackground(turns[0]);
		
		turnText = new JLabel(names[turnCount%names.length] + " is playing.");
		add(turnText, BorderLayout.SOUTH);
		
		setSize(750,750);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setVisible(true);
	}
	
	@Override
	public Color getDefaultColor() {
		// TODO Auto-generated method stub
		return Color.GREEN;
	}

	@Override
	public Color getTurn() {
		// TODO Auto-generated method stub
		return turns[turnCount%turns.length];
	}
	
	public void updateTurn(int layer){
		if(layer == turns.length)
			handleWinner();
		else{
			turnCount++;
			for(Tile[] t: board){
				for(Tile ti: t){
					if(ti.isClickable()){
						turnText.setText(names[turnCount%names.length] + " is playing.");
						return;
					}	
				}
			}
			updateTurn(layer+1);
		}	
		
	}
	
	public void handleWinner(){
		int [] tileCounts = new int[turns.length];
		for(Tile[] row: board){
			for(Tile t: row){
				int index = getIndex(turns,t.getBackground());
				if(index >= 0)
					tileCounts[index]++;
			}
		}
		
		int maxCountIndex = 0;
		for(int i = 1; i < tileCounts.length; i++){
			if(tileCounts[i] > tileCounts[maxCountIndex])
				maxCountIndex = i;
		}
		
		int choice = JOptionPane.showConfirmDialog(this, names[maxCountIndex] + " has won!\n"
				+ "Press OK to rematch!",
				"Game over!",
				JOptionPane.OK_CANCEL_OPTION,
				JOptionPane.INFORMATION_MESSAGE);
		
		Game g;
		if(choice == JOptionPane.OK_OPTION)
			g = new Game(turns,names);
		this.dispose();
	}
	
	private static int getIndex(Object [] arry, Object find){
		for(int i = 0; i < arry.length; i++)
			if(arry[i].equals(find))
				return i;
		return -1;
	}
	

	@Override
	public Tile[][] getGrid() {
		// TODO Auto-generated method stub
		return board;
	}
	
	public static void main(String[] args){
		Game g = new Game(new Color[]{Color.WHITE,Color.BLACK},
				new String[]{"White","Black"});
	}

}
