package yahtzee;

import java.util.*;
import javax.swing.*;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.*;

public class DicePanel extends JPanel implements ActionListener{

	
	private JButton [] dice;
	private DiceRoll dr;
	private Set<Integer> exclusions;
	
	public DicePanel() {
		dr = new DiceRoll();
		dice = new JButton[DiceRoll.NUM_DICE];
		for(int i = 0; i < DiceRoll.NUM_DICE; i++){
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
		dr.rollDice(exclusions);
		drawButtons();
			
	}
	
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
	
	public void clearExclusions(){
		exclusions = new HashSet<Integer>();
	}


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
		return dr.getDice();
	}
	
	public static void main(String [] args){
		final DicePanel dr = new DicePanel();
		JFrame win = new JFrame("Dice test");
		win.add(dr);
		JButton roll = new JButton("Roll");
		//roll.addActionListener((ActionEvent e) -> dr.rollDice());
		win.add(roll,BorderLayout.SOUTH);
		win.setVisible(true);
		win.setSize(500, 125);
	}

}
