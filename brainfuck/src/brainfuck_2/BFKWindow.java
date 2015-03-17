package brainfuck_2;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.*;
import java.io.IOException;
import java.util.HashMap;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.filechooser.FileNameExtensionFilter;

public class BFKWindow extends JFrame implements ActionListener, BfkUIHandler {

	//UI components
	private JTextArea code;
	private JTextArea output;
	private JCheckBox inputType;
	private JButton run;
	private JButton debug;
	private JButton load;
	
	//File IO
	private FileIO io;
	
	/**
	 * Makes the window.
	 */
	public BFKWindow(FileIO i){
		super("Brainfuck interpreter!");
		io = i;
		setUpWindow();
	}
	
	/**
	 * Sets up everything within the window (called in construction).
	 */
	private void setUpWindow(){
		//Sets size and layout
		setSize(500,500);
		setLayout(new GridLayout(2,2));
		
		//Sets up all components
		code = new JTextArea("Enter your code here!");
		output = new JTextArea();
		output.setEditable(false);
		inputType = new JCheckBox("Select to input and output integers");
		run = new JButton("Run");
		debug = new JButton("Debug");
		load = new JButton("Load or save");
		
		//Gives a border to the text fields
		Border bord = BorderFactory.createLineBorder(Color.BLACK);
		Border empt = BorderFactory.createEmptyBorder(10,10,10,10);
		code.setBorder(BorderFactory.createCompoundBorder(bord,empt));
		output.setBorder(BorderFactory.createCompoundBorder(bord,empt));
		
		//Adds action listeners to the buttons
		run.addActionListener(this);
		debug.addActionListener(this);
		load.addActionListener(this);
		
		//Adds every component
		add(code);
		add(output);
		add(inputType);
		add(run);
		add(debug);
		add(load);
		
		//Sets close operation and makes the window visible
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setVisible(true);
	}

	@Override
	/**
	 * Handles the output from a bfk interpreter.
	 * @param val the value to output
	 */
	public void output(int val) {
		//Ensure that the type of output matches user's selection.
		if(inputType.isSelected())
			output.setText(output.getText() + val + " ");
		else
			output.setText(output.getText() + (char)val);
	}

	@Override
	/**
	 * Get the input from the user
	 * @return the input
	 */
	public int getInput(){
		//Ensure that the input matches the type selected.
		if(inputType.isSelected())
			return getIntInput();
		else
			return getCharInput();
	}
	
	/**
	 * Get an integer from the user
	 * @return the integer the user inputted.
	 */
	private int getIntInput(){
		do{
			//Prompt the user
			String toRet = JOptionPane.showInputDialog(this,"Enter an integer input...");
			try{
				//Try to return their int
				return Integer.parseInt(toRet);
			}catch(Exception e){
				//Closing is considered a 0.
				if(toRet == null)
					return 0;
			}
		}while(true);
	}
	
	/**
	 * Get a character from the user.
	 * @return the character the user wanted.
	 */
	private int getCharInput(){
		String toRet;
		do{
			//Prompt the user
			toRet = JOptionPane.showInputDialog(this,"Enter a character input...");
			//Closing or cancel is considered a 0.
			if(toRet == null)
				return 0;
		}while(toRet.length() != 1);
		return toRet.charAt(0);
	}

	@Override
	/**
	 * Show the debug information to the user
	 * @param tape the memory
	 * @param pointer the location in memory
	 * @param readIndex the index in the code
	 * @return whether to continue running
	 */
	public boolean debug(HashMap<Integer, Integer> tape, int pointer, int readIndex) {
		//Constructs a message with:
		//The current command.
		//The tape.
		//The current point in the tape.
		String message = "Current code: " + code.getText().charAt(readIndex) + '\n';
		message += "The tape (all other values are 0):\n" + printMap(tape);
		message += "\nThe current pointer (addr: " + pointer + "): ";
		if(!tape.containsKey(pointer))
			message += '0';
		else
			message += tape.get(pointer);
		message += "\nClick OK to continue, cancel to stop execution.";
		return JOptionPane.showConfirmDialog(this, message, "Debugging",
				JOptionPane.CANCEL_OPTION,
				JOptionPane.INFORMATION_MESSAGE) == JOptionPane.YES_OPTION;
	}
	
	/**
	 * Get the contents of the tape as a table-like string
	 * @param map the tape
	 * @return the table-like string
	 */
	private String printMap(HashMap<Integer,Integer> map){
		//Set up the lines
		String upperLine = "Address: ";
		String lowerLine = "Value:     ";
		
		//Loop through each key, adding the integers
		for(Integer i: map.keySet()){
			upperLine += i + " ";
			lowerLine += map.get(i) + " ";
			
			//Make sure that the strings are even (attempted).
			if(lowerLine.length() > upperLine.length())
				for(int j = upperLine.length(); j < lowerLine.length(); j++)
					upperLine += " ";
			else if(lowerLine.length() < upperLine.length())
				for(int j = lowerLine.length(); j < upperLine.length(); j++)
					lowerLine += " ";
		}
		
		return upperLine + '\n' + lowerLine;
	}

	@Override
	/**
	 * Inform the user of an error
	 */
	public void showError() {
		JOptionPane.showMessageDialog(this, "There seems to be an infinite loop!");
	}
	
	public void endOfCode(){
		JOptionPane.showMessageDialog(this, output.getText(),
				"The output:", JFrame.DISPOSE_ON_CLOSE);
		output.setText("");
	}

	@Override
	/**
	 * Handle button presses.
	 * @param arg0 the button press event.
	 */
	public void actionPerformed(ActionEvent arg0) {
		if(arg0.getSource() == run)
			ReadBfk.readAll(code.getText(), this);
		else if(arg0.getSource() == debug)
			ReadBfk.debug(code.getText(), this);
		else if(arg0.getSource() == load)
			handleLoad();
	}
	
	public void handleLoad(){
		JFileChooser jfc = new JFileChooser();
		FileNameExtensionFilter f = new FileNameExtensionFilter("Text and own .bf format",
				"txt","bf");
		jfc.setFileFilter(f);
		
		int approved; 
		boolean open = code.getText().trim().equals("");
		if(open)
			approved = jfc.showOpenDialog(this);
		else
			approved = jfc.showSaveDialog(this);
		
		if(approved != JFileChooser.APPROVE_OPTION)
			return;
		
		try{
			io.setFile(jfc.getSelectedFile().getAbsolutePath());
			if(open)
				code.setText(io.readFile());
			else
				io.writeFile(code.getText());
		}catch(IOException e){
			JOptionPane.showMessageDialog(this,"Load failed.");
		}
	}

	/**
	 * Run the window
	 * @param args not used.
	 */
	public static void main(String[] args) {
		BFKWindow win = new BFKWindow(new BFKFileIO());
	}

}
