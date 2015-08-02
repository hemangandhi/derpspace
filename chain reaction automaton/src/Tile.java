//Generic class for each tile
import javax.swing.JLabel;
import java.awt.event.*;

public class Tile implements MouseListener{

	//game board
	public Board game;
	//Attached JLabel
	public JLabel display;
	//number of horizontally or vertically adjacent tiles
	public int numNeighbors;
	//number of 'units' in this tile
	private int population = 0;
	//Location of each neighbor
	public int [] neighborsX;
	public int [] neighborsY;
	//Overflowed?
	public boolean overflowed = false;
	//Bad value?
	public boolean badValue = false;
	
	//constructor
	public Tile(Board b, int [] neighborX, int [] neighborY, JLabel toBDisp){
		game = b;
		numNeighbors = neighborX.length;
		neighborsX = neighborX;
		neighborsY = neighborY;
		display = toBDisp;
		//set text to initial population
		display.setText(""+population);
		display.addMouseListener(this);
	}
	
	public void add(){
		if(badValue){
			//reset population
			population = 0;
			badValue = false;
		}else if(overflowed){
			//do nothing if overflowed
			return;
		}else{	
			//add one to people
			population++;
			//explode as much as possible
			if(population == numNeighbors)
				explode();
		}
		//Display it
		setUpLabel();
	}
	
	public void explode(){
		//take away from population
		population -= numNeighbors;
		for(int i = 0; i < numNeighbors; i++){
			//add one unit to each neighbor
			game.grid[neighborsX[i]][neighborsY[i]].add();
		}
	}
	
	public void setPopulation(int pop){
		//set the population
		population = pop;
		display.setText(""+pop);
		//loop for every explosion
		try{
			while(population >= numNeighbors)
				explode();
		}catch(StackOverflowError stack){
			overflowed = true;
		}
		setUpLabel();
	}
	
	private void setUpLabel(){
		if(overflowed){
			//show overflow
			display.setText("Overflowed!");
			display.setBackground(java.awt.Color.WHITE);
		}else{
			//try to show population
			display.setText(""+population);
			try{
				//and colored background
				if(population==0)
					display.setBackground(java.awt.Color.WHITE);
				else
					display.setBackground(new java.awt.Color((255/numNeighbors)*population,0,255));
			}catch(IllegalArgumentException ill){
				//number was too big (population)
				badValue = true;
				display.setText(population+": bad value; restart");
				display.setBackground(java.awt.Color.WHITE);
			}
		}
	}

	public void mouseClicked(MouseEvent arg0) {
		//add();
		
	}

	public void mouseEntered(MouseEvent arg0) {
		//Green tile on hover
		display.setBackground(java.awt.Color.GREEN);
		
	}

	public void mouseExited(MouseEvent arg0) {
		//reset background on exit
		setUpLabel();
		
	}

	public void mousePressed(MouseEvent arg0) {
		// nothing
		
	}

	public void mouseReleased(MouseEvent arg0) {
		try{
			add();
		}catch(StackOverflowError stack){
			overflowed = true;
			setUpLabel();
		}
	}
	
}
