package yahtzee;

import java.util.*;
import javax.swing.*;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.*;

public class DiceRoll extends JPanel implements ActionListener{

	public static final int NUM_DICE = 5;
	public static final int DICE_SIDES = 6;
	
	private JButton [] dice;
	private int [] roll;
	private Set<Integer> exclusions;
	
	public DiceRoll() {
		roll = new int[NUM_DICE];
		dice = new JButton[NUM_DICE];
		for(int i = 0; i < NUM_DICE; i++){
			dice[i] = new JButton();
			dice[i].setBackground(Color.RED);
			dice[i].addActionListener(this);
			add(dice[i]);
		}	
		exclusions = new HashSet<Integer>();
		rollDice();
		setSize(500,100);
	}
	
	public void rollDice(){
		for(int i = 0; i < roll.length; i++)
			if(!exclusions.contains(i))
				roll[i] = (int)((Math.random()*DICE_SIDES) + 1);
		drawButtons();
			
	}
	
	private void drawButtons(){
		for(int i = 0; i < roll.length; i++){
			dice[i].setText(roll[i] + " ");
			if(exclusions.contains(i))
				dice[i].setBackground(Color.GREEN);
			else
				dice[i].setBackground(Color.RED);
		}
	}
	
	public void clearExclusions(){
		exclusions = new HashSet<>();
	}

	@Override
	public void actionPerformed(ActionEvent arg0) {
		for(int i = 0; i < dice.length; i++)
			if(dice[i] == arg0.getSource()){
				if(exclusions.contains(i))
					exclusions.remove(i);
				else
					exclusions.add(i);
			}	
		drawButtons();
	}
	
	public int [] getDice(){
		return roll;
	}
	
	public static void main(String [] args){
		final DiceRoll dr = new DiceRoll();
		JFrame win = new JFrame("Dice test");
		win.add(dr);
		JButton roll = new JButton("Roll");
		roll.addActionListener((ActionEvent e) -> dr.rollDice());
		win.add(roll,BorderLayout.SOUTH);
		win.setVisible(true);
		win.setSize(500, 125);
	}

}
