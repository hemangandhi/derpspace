package yahtzee;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.*;

public class PlayerPanel extends JPanel implements ActionListener {
	
	private UI ui;
	private Player me;
	
	private JButton [] actions;
	private JTextField [] points;
	
	private JLabel upper, lower, overall;
	private JTextField upperTotal, lowerTotal, overallTotal;
	
	private int rollsLeft;
	
	/**
	 * Makes an empty panel within the specified UI.
	 * @param screen the UI this panel is a part of.
	 */
	public PlayerPanel(UI screen) {
		rollsLeft = 2;
		ui = screen;
		YahtzeeEntry[] entries = YahtzeeEntry.values();
		me = new Player();
		
		setLayout(new GridLayout(entries.length + 3,2));
		
		actions = new JButton[entries.length];
		points = new JTextField[actions.length];
		for(int i = 0; i < points.length; i++){
			actions[i] = new JButton(entries[i].toString());
			actions[i].addActionListener(this);
			actions[i].setEnabled(false);
			add(actions[i]);
			points[i] = new JTextField();
			points[i].setEditable(false);
			points[i].setFont(new Font("Luciana Grande",Font.BOLD,20));
			add(points[i]);
		}
		
		upper = new JLabel("Upper total: ");
		lower = new JLabel("Lower total: ");
		overall = new JLabel("Overall total: ");
		upperTotal = new JTextField();
		lowerTotal = new JTextField();
		overallTotal = new JTextField();
		upperTotal.setEditable(false);
		lowerTotal.setEditable(false);
		overallTotal.setEditable(false);
		upperTotal.setFont(new Font("Luciana Grande",Font.BOLD,20));
		lowerTotal.setFont(new Font("Luciana Grande",Font.BOLD,20));
		overallTotal.setFont(new Font("Luciana Grande",Font.BOLD,20));
		
		add(upper);add(upperTotal);
		add(lower);add(lowerTotal);
		add(overall);add(overallTotal);
		
		setSize(250,750);
		
	}
	
	/**
	 * Gives the rolls left in a players turn.
	 * @return the rolls left in a players turn.
	 */
	public int numRolls(){
		return rollsLeft;
	}
	
	/**
	 * Updates all the text fields, returning the player's overall total.
	 * @return the player's overall total.
	 */
	public int updateText(){
		for(int i = 0; i < points.length; i++){
			String init = points[i].getText();
			points[i].setText(me.getScore(YahtzeeEntry.getByString(actions[i].getText())) + (init.endsWith("forced")?" forced":""));
		}
		
		upperTotal.setText(me.getUpperScore() + "");
		lowerTotal.setText(me.getLowerScore() + "");
		overallTotal.setText(me.getTotalScore() + "");
		
		for(JButton jb: actions)
			jb.setEnabled(false);
		
		return me.getTotalScore();
	}


	/**
	 * Handles a selection of a particular yahtzee entry.
	 */
	public void actionPerformed(ActionEvent arg0) {
		JButton clicked = (JButton) arg0.getSource();
		
		for(int i = 0; i < actions.length; i++)
			if(actions[i] == clicked && clicked.isEnabled()){
				YahtzeeEntry currEntry = YahtzeeEntry.getByString(clicked.getText());
				int score = YahtzeeEntry.getVal(currEntry,ui.getDiceRoll());
				points[i].setText("" + score);
				me.setScore(currEntry, score);
				clicked.setEnabled(false);
			}
		
		updateText();
		
		ui.nextTurn();
	}
	
	/**
	 * Handles the forced joker rule.
	 */
	private void handleForcedJoker(){
		DicePanel rolled = ui.getDiceRoll();
		int val = rolled.getDice()[0];
		
		me.setScore(YahtzeeEntry.YAHTZEE, (me.timesYahtzeed() + 1)*50);
		
		if(!me.isUsed(YahtzeeEntry.upperValues()[val - 1])){
			me.setScore(YahtzeeEntry.upperValues()[val - 1], val*5);
			points[val - 1].setText(val*5 + " forced");
			ui.nextTurn();
		}
		
		updateText();
		
			
	}
	
	/**
	 * Updates the tool tips that give the user the potential scores.
	 * @param dr the rolled dice.
	 */
	private void updateTooltips(DicePanel dr){
		for(JButton jb: actions){
			jb.setToolTipText(""+YahtzeeEntry.getVal(YahtzeeEntry.getByString(jb.getText()), dr));
		}
	}
	
	/**
	 * Handles a dice roll.
	 * @return the number of rolls left after this roll.
	 */
	public int rollDice(){
		ui.getDiceRoll().rollDice();
		updateTooltips(ui.getDiceRoll());
		if(YahtzeeEntry.getVal(YahtzeeEntry.YAHTZEE, ui.getDiceRoll()) > 0 && me.timesYahtzeed() >= 1)
			handleForcedJoker();
		return --rollsLeft;
	}
	
	/**
	 * Handles a new turn, enabling the possible buttons.
	 * @return whether the player had anything to play.
	 */
	public boolean handleNewTurn(){
		boolean toRet = false;
		for(int i = 0; i < actions.length; i++){
			boolean toSet = !me.isUsed(YahtzeeEntry.getByString(actions[i].getText()));
			if(toSet) 
				toRet = true;
			actions[i].setEnabled(toSet);
		}	
		rollsLeft = 3;
		rollDice();
		return toRet;
	}

}
