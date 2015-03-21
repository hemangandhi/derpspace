package chess;

import javax.swing.*;

public class Tester {

	public static void main(String[] args) {
		JFrame win = new JFrame("Chess");
		win.add(Board.chessBoard());
		win.setVisible(true);
		win.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		win.setSize(700, 700);

	}

}
