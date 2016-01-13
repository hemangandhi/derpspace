import interpreter.*;

import javax.swing.JOptionPane;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.LinkedList;

import javax.imageio.*;

public class TestIO implements PietIO{

	public static void main(String[] args) throws IOException {
		// TODO Auto-generated method stub
		BufferedImage i = ImageIO.read(new File("test3.png"));
		PictureReader.read(i, new TestIO(), true);
	}
	
	private LinkedList<Integer> st;
	
	public TestIO(){
		st = new LinkedList<>();
	}

	@Override
	public int in(boolean isChar) {
		// TODO Auto-generated method stub
		if(isChar)
			return getC();
		else
			return getI();
	}
	
	private int getC(){
		String inp = "";
		do{
			inp = JOptionPane.showInputDialog("Enter a char!");
		}while(inp.length() != 1);
		return inp.charAt(0);
	}
	
	private int getI(){
		do{
			String i = JOptionPane.showInputDialog("Enter an int!");
			try{
				return Integer.parseInt(i);
			}catch(NumberFormatException nfe){
				
			}
		}while(true);
	}

	@Override
	public void out(int v, boolean charOut) {
		// TODO Auto-generated method stub
		if(charOut)
			System.out.print((char) v);
		else
			System.out.print(v);
	}

	@Override
	public boolean debug(Coord curr, Direction dc, Direction cc) {
		// TODO Auto-generated method stub
		return JOptionPane.OK_OPTION ==
				JOptionPane.showConfirmDialog(null, 
						"Currently at" + curr + " pointing " + dc + " and " + cc,
						"Debug",
						JOptionPane.OK_CANCEL_OPTION,
						JOptionPane.INFORMATION_MESSAGE);
	}

	@Override
	public void handleEnd(Coord end, Direction dc, Direction cc) {
		System.out.println("\n >>> Program end!");
	}

	@Override
	public int popStack() {
		// TODO Auto-generated method stub
		return st.pop();
	}

	@Override
	public void pushStack(int v) {
		// TODO Auto-generated method stub
		st.push(v);
	}

	@Override
	public void rollStack(int rolls, int depth) {
		// TODO Auto-generated method stub
		for(int i = 0; i < rolls; i++){
			Integer v = st.pop();
			st.add(Math.min(st.size(), depth), v);
		}
	}

}
