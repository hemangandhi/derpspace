package yahtzee;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

public class Player extends JPanel implements ActionListener {
	
	private UI ui;
	
	private JButton [] actions;
	private JTextField [] points;
	
	private JLabel upper, lower, overall;
	private JTextField upperTotal, lowerTotal, overallTotal;
	
	private int rollsLeft;
	
	public Player(UI screen) {
		rollsLeft = 2;
		ui = screen;
		YahtzeeEntry[] entries = YahtzeeEntry.values();
		
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
		add(upper);add(upperTotal);
		add(lower);add(lowerTotal);
		add(overall);add(overallTotal);
		
		setSize(250,750);
		
	}
	
	public int numRolls(){
		return rollsLeft;
	}

	@Override
	public void actionPerformed(ActionEvent arg0) {
		JButton clicked = (JButton) arg0.getSource();
		
		for(int i = 0; i < actions.length; i++)
			if(actions[i] == clicked && clicked.isEnabled()){
				points[i].setText("" + YahtzeeEntry.getVal(YahtzeeEntry.getByString(clicked.getText()),
						ui.getDiceRoll()));
				clicked.setEnabled(false);
			}
		
		int i, sum = 0;
		for(i = 0; i < 5; i++)
			try{
				sum += Integer.parseInt(points[i].getText());
			}catch(NumberFormatException nfe){
				sum += 0;
			}
		upperTotal.setText(sum+"");
		sum = 0;
		for(; i < actions.length; i++)
			try{
				sum += Integer.parseInt(points[i].getText());
			}catch(NumberFormatException nfe){
				sum += 0;
			}
		lowerTotal.setText(sum+"");
		int upperSum = Integer.parseInt(upperTotal.getText());
		if(upperSum >= 63)
			upperSum += 35;
		overallTotal.setText((sum + upperSum) + "");
		
		for(JButton jb: actions)
			jb.setEnabled(false);
		
		ui.nextTurn();
	}
	
	private void updateTooltips(DiceRoll dr){
		for(JButton jb: actions){
			jb.setToolTipText(""+YahtzeeEntry.getVal(YahtzeeEntry.getByString(jb.getText()), dr));
		}
	}
	
	public int rollDice(){
		ui.getDiceRoll().rollDice();
		updateTooltips(ui.getDiceRoll());
		return --rollsLeft;
	}
	
	public void handleNewTurn(){
		for(int i = 0; i < actions.length; i++)
			actions[i].setEnabled(points[i].getText().length() <= 0);
		ui.getDiceRoll().rollDice();
		updateTooltips(ui.getDiceRoll());
		rollsLeft = 2;
	}

}
