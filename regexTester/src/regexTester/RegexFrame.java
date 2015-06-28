package regexTester;

import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.util.regex.*;
import java.awt.event.*;

public class RegexFrame extends JFrame{

	private JTextArea input;
	private JTextArea output;
	private JTextField pattern;
	private JButton submit;
	
	public RegexFrame(){
		super("Regex Tester!");
		
		input = new JTextArea("Enter any string to test...");
		output = new JTextArea("Matches will display here...");
		pattern = new JTextField("Enter a regex here!");
		submit = new JButton("Submit!");
		
		output.setEditable(false);
		input.setLineWrap(true);
		
		setLayout(new GridLayout(2,2));
		add(input);
		add(output);
		add(pattern);
		add(submit);
		
		submit.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				displayMatches();
			}
		});
		
		input.setBorder(new LineBorder(Color.BLACK));
		
		setVisible(true);
		setSize(500, 500);
	}
	
	public void displayMatches(){
		output.setText("");
		
		Matcher m = Pattern.compile(pattern.getText()).matcher(input.getText());
		while(m.find())
			output.append("\"" + m.group() + "\"" + " at " + m.start() + "\n");
	}
	
	public static void main(String [] args){
		RegexFrame rf = new RegexFrame();
	}
	
	
}
