//Generic class for board and GUI implementation
import javax.swing.*;
import java.awt.*;
import javax.swing.border.LineBorder;

public class Board extends JFrame{

	/**
	 * This is bullshit!
	 */
	private static final long serialVersionUID = 1L;
	//The tile grid
	public Tile[][] grid;
	//The visible grid
	public JLabel [][] GUI;
	//Other GUI components
	public GridLayout layout;
	
	//Constructor
	public Board(int height, int width, boolean diag){
		//Construct a JFrame
		super("Chain Reaction");
		setUpGUI(height,width);
		//Instantiate [][]'s
		grid = new Tile[width][height];
		GUI = new JLabel[width][height];
		for(int i = 0; i < width; i++){
			for(int j = 0; j < height; j++){
				//For each tile on the grid...
				//Set up JLabel
				GUI[i][j] = new JLabel();
				GUI[i][j].setOpaque(true);
				GUI[i][j].setBackground(Color.WHITE);
				GUI[i][j].setBorder(new LineBorder(Color.BLACK));
				//Neighbor arrays
				int [] xNums = new int [8]; 
				int [] yNums = new int [8];
				int indexCount = 0;
				//For every neighboring tile
				for(int ia = -1; ia < 2; ia++){
					for(int ja = -1; ja < 2; ja++){
						//ensure this is not a disallowed diagonal
						if(Math.abs(ja)== Math.abs(ia) && !diag){
							continue;
						//ensure this is not the tile itself	
						}else if(ia ==0 && ja==0){
							continue;
						//ensure that this is on the grid	
						}else if(ia + i >= width || ja + j >= height ){
							continue;	
						}else if(ia + i < 0 || ja + j < 0){
							continue;
						//assign the location	
						}else{
							xNums[indexCount] = i + ia;
							yNums[indexCount] = j + ja;
							//keep a tab of how many
							indexCount++;
						}
					}
				}
				//filter out any zeroes
				xNums = filtered(indexCount,xNums);
				yNums = filtered(indexCount,yNums);
				//make a Tile
				grid[i][j] = new Tile(this,xNums,yNums,GUI[i][j]);
				//add the JLabel
				add(GUI[i][j]);
			}
		}
	}
	
	public void setUpGUI(int height, int width){
		//Set up JFrame settings
		layout = new GridLayout(height,width);
		setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		setSize(1000,1000);
		setVisible(true);
		setLayout(layout);
	}
	
	private int[] filtered(int until, int[] original){
		//Copy a list until until
		int [] toRet = new int [until];
		for(int i = 0; i < until; i++){
			toRet[i] = original[i];
		}
		return toRet;
	}
	
	public void randomSpawn(){
		for(int i = 0; i < grid.length; i++){
			for(int j = 0; j < grid[i].length; j++){
				int numN = grid[i][j].numNeighbors;
				grid[i][j].setPopulation((int)(Math.random()*numN+1));
			}
		}
	}
	
	public static void main(String [] args){
		Board b = new Board(10,10,false);
		//b.randomSpawn();
	}
	
}
