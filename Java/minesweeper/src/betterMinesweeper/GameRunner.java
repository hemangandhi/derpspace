package betterMinesweeper;

import javax.swing.JOptionPane;

public class GameRunner {

	public static void runGame(){
		int boardH = getInt("Type in the width of the board.");
		int boardW = getInt("Type int the height.");
		int mines = getInt("Type in the number of mines.");
		
		Board b = new Board(boardH, boardW, mines);
	}
	
	public static int getInt(String prompt){
		do{
			String toRet = JOptionPane.showInputDialog(prompt);
			try{
				return Integer.parseInt(toRet);
			}catch(NumberFormatException nfe){
				prompt = "That should be an integer, not " + toRet;
			}
		}while(true);
	}

	public static void main(String[] args) {
		runGame();

	}

}
