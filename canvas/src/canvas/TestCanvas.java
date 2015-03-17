package canvas;

import javax.swing.*;

public class TestCanvas {

	public static void runCanvas(){
		JFrame window = new JFrame("Canvas!");
		window.setSize(1000, 1000);
		window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		Canvas c = new Canvas();
		c.addMouseListener(c);
		window.add(c);
		
		Thread t = new Thread(c);
		t.start();
		
		window.setVisible(true);
	}
	
	public static void main(String[] args) {
		runCanvas();
	}

}
