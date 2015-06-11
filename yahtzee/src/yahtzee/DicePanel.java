package yahtzee;

import java.util.*;

import javax.swing.*;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.*;

public class DicePanel extends JPanel implements ActionListener{

	
	private JButton [] dice;
	private DiceRoll dr;
	private Set<Integer> exclusions;
	
	/**
	 * Makes a new panel instance.
	 */
	public DicePanel() {
		dr = new DiceRoll();
		dice = new JButton[DiceRoll.NUM_DICE];
		for(int i = 0; i < DiceRoll.NUM_DICE; i++){
			dice[i] = new JButton();
			dice[i].setFont(new Font("Luciana Grande",Font.BOLD,20));
			dice[i].setBackground(Color.RED);
			dice[i].addActionListener(this);
			add(dice[i]);
		}	
		exclusions = new HashSet<Integer>();
		rollDice();
		setSize(500,200);
	}
	
	/**
	 * Rolls the dice.
	 */
	public void rollDice(){
		dr.rollDice(exclusions);
		drawButtons();
			
	}
	
	/**
	 * Draws each die as a button.
	 * Green if it's excluded and red otherwise.
	 */
	private void drawButtons(){
		int [] roll = dr.getDice();
		for(int i = 0; i < roll.length; i++){
			dice[i].setText(roll[i] + " ");
			if(exclusions.contains(i))
				dice[i].setBackground(Color.GREEN);
			else
				dice[i].setBackground(Color.RED);
		}
	}
	
	/**
	 * Resets the excluded die, useful at the end of a player's turn.
	 */
	public void clearExclusions(){
		exclusions = new HashSet<Integer>();
	}

	/**
	 * Handles die selection.
	 */
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
	
	/**
	 * Returns the roll from dice roll.
	 * @return the current integer each dice shows.
	 */
	public int [] getDice(){
		return dr.getDice();
	}
	
//	public static void main(String [] args){
//		final DicePanel dr = new DicePanel();
//		JFrame win = new JFrame("Dice test");
//		win.add(dr);
//		JButton roll = new JButton("Roll");
//		//roll.addActionListener((ActionEvent e) -> dr.rollDice());
//		win.add(roll,BorderLayout.SOUTH);
//		win.setVisible(true);
//		win.setSize(500, 125);
//	}   was for testing the dice...

}
