package yahtzee;

import java.awt.BorderLayout;
import java.awt.GridLayout;

import javax.swing.*;

import java.awt.event.*;

public class UI extends JFrame implements ActionListener{

	private Player [] players;
	private DiceRoll dice;
	private int currPlayer;
	private JButton roll;
	
	public UI(int numPlayers){
		super("Yahtzee!");
		
		currPlayer = -1;
		players = new Player[numPlayers];
		dice = new DiceRoll();
		add(dice, BorderLayout.NORTH);
		
		JPanel playersPane = new JPanel();
		playersPane.setLayout(new GridLayout(1,numPlayers));
		for(int i = 0; i < numPlayers; i++){
			players[i] = new Player(this);
			playersPane.add(players[i]);
		}
		add(new JScrollPane(playersPane));
		
		roll = new JButton("Roll");
		roll.addActionListener(this);
		add(roll,BorderLayout.SOUTH);
		
		setSize(1000,1000);
		setVisible(true);
		
		nextTurn();
	}
	
	public DiceRoll getDiceRoll(){
		return dice;
	}
	
	public void nextTurn(){
		currPlayer++;
		currPlayer %= players.length;
		dice.clearExclusions();
		players[currPlayer].handleNewTurn();
		roll.setText("Rolls (" + players[currPlayer].numRolls() + ")");
		roll.setEnabled(true);
	}

	@Override
	public void actionPerformed(ActionEvent arg0) {
		int rolls = players[currPlayer].rollDice();
		if(rolls <= 0)
			roll.setEnabled(false);
		roll.setText("Rolls (" + players[currPlayer].numRolls() + ")");
	}
	
	public static void main(String [] args){
		UI u = new UI(4);
		
	}
}
