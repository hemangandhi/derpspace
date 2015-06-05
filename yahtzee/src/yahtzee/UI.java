package yahtzee;

import java.awt.BorderLayout;
import java.awt.GridLayout;

import javax.swing.*;

import java.awt.event.*;

public class UI extends JFrame implements ActionListener{

	private PlayerPanel [] players;
	private DicePanel dice;
	private int currPlayer;
	private int finshes;
	private JButton roll;
	
	public UI(int numPlayers){
		super("Yahtzee!");
		
		currPlayer = -1;
		players = new PlayerPanel[numPlayers];
		dice = new DicePanel();
		add(dice, BorderLayout.NORTH);
		
		JPanel playersPane = new JPanel();
		playersPane.setLayout(new GridLayout(1,numPlayers));
		for(int i = 0; i < numPlayers; i++){
			players[i] = new PlayerPanel(this);
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
	
	public DicePanel getDiceRoll(){
		return dice;
	}
	
	public void nextTurn(){
		currPlayer++;
		currPlayer %= players.length;
		dice.clearExclusions();
		boolean didFin = !players[currPlayer].handleNewTurn();
		roll.setText("Rolls (" + players[currPlayer].numRolls() + ")");
		roll.setEnabled(true);
		if(didFin){
			finshes++;
			if(finshes < players.length)
				nextTurn();
			else
				endGame();
		}	
	}
	
	public void endGame(){
		UI u;
		String mes = "Game over!\n";
		for(int p = 0; p < players.length; p++){
			mes += "Player " + p + " got " + players[p].updateText();
		}
		//Prompt user for continuation
		boolean toContinue = JOptionPane.showConfirmDialog(this, mes, "resume?", 
				JOptionPane.CANCEL_OPTION, JOptionPane.INFORMATION_MESSAGE) == 0;
		//Get rid of the game window
		this.dispose();
		//If the user wanted to continue, repeat
		if(toContinue)
			u = new UI(players.length);
	}


	public void actionPerformed(ActionEvent arg0) {
		int rolls = players[currPlayer].rollDice();
		if(rolls <= 0)
			roll.setEnabled(false);
		roll.setText("Rolls (" + players[currPlayer].numRolls() + ")");
	}
	
	public static void main(String [] args){
		UI u = new UI(100);
		
	}
}
