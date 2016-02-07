package swingUI;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;
import javax.swing.border.*;
import general.*;

public class GeneralFrame<T> extends JFrame implements ActionListener{

	private JLabel[][] mat;
	private JButton solve;
	private SwingValidator<T> val;
	
	public GeneralFrame(SwingValidator<T> val){
		super(val.title());
		this.val = val;
		initialize();
	}
	
	private void initialize(){
		solve = new JButton("solve");
		solve.addActionListener(this);
		
		int x, y;
		for(x = promptForInt("Enter the row count:"),
				y = promptForInt("Enter the column count:");
				!val.isValidDim(x, y);
				x = promptForInt("Invalid row count..."),
				y = promptForInt("Invalid column count..."));
		JPanel top = new JPanel();
		top.setLayout(new GridLayout(x, y));
		mat = new JLabel[x][y];
		for(int i = 0; i < x; i++)
			for(int j = 0; j < y; j++){
				mat[i][j] = new JLabel();
				val.render(mat[i][j], null);
				mat[i][j].addMouseListener(val);
				mat[i][j].setBorder(new LineBorder(Color.BLACK));
				top.add(mat[i][j]);
			}
		
		add(top);
		add(solve, BorderLayout.SOUTH);
		setVisible(true);
		setSize(50*x, 50*y);
	}
	
	private int promptForInt(String prompt){
		String s;
		do{
			s = JOptionPane.showInputDialog(prompt);
			try{
				return Integer.parseInt(s);
			}catch(NumberFormatException nfe){
				prompt += " Enter an int!";
			}
		}while(true);
	}

	@Override
	public void actionPerformed(ActionEvent arg0) {
		T[][] kn = (T [][]) new Object[mat.length][mat[0].length];
		
		for(int i = 0; i < kn.length; i++)
			for(int j = 0; j < kn[0].length; j++){
				kn[i][j] = val.labelToState(mat[i][j]);
			}
		
		kn = Solver.solve(kn, val);
		for(int i = 0; i < kn.length; i++)
			for(int j = 0; j < kn[i].length; j++)
				val.render(mat[i][j], kn[i][j]);
	}
}
