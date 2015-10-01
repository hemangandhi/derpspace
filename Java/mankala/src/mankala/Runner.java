package mankala;

import java.util.Scanner;

public class Runner {
	
	public static void run(Scanner s){
		Game g = new Game();
		while(!g.gameOver()){
			printState(g);
			
			System.out.print("$");
			for(int i = 0; i < g.turn(); i++)
				System.out.print("\t");
			
			for(int i = 0; i < 6; i++)
				System.out.print(i + "\t");
			
			System.out.println();
			System.out.println("Pick a cell to play, as enumerated.");
			int choice = chooseInt(0,5,s);
			if(g.turn() == 0)
				g.play(choice);
			else
				g.play(5 - choice);
		}
		
		switch(g.winState()){
		case -1:
			System.out.println("It was a tie!");
			break;
		case 0:
			System.out.println("First player won!");
			break;
		case 1:
			System.out.println("Second player won!");
		}
	}
	
	public static int chooseInt(int bot, int top, Scanner toUse){
		int r;
		do{
			System.out.println("Enter a number between " + bot + " and " + top);
			try{
				r = Integer.parseInt(toUse.nextLine());
			}catch(NumberFormatException nfe){
				System.out.println("Enter a number.");
				r = bot - 1;
			}
		}while(r < bot || r > top);
		return r;
	}
	
	public static void printState(Game g){
		int[] state = g.getBoard();
		System.out.print(((g.turn() == 0)?">":" "));
		for(int i = 0; i < 7; i++){
			System.out.print(state[i] + ((i == 6)?"":"\t"));
		}
		System.out.println();
		System.out.print(((g.turn() == 1)?">":" "));
		for(int j = 1; j <= 7; j++){
			System.out.print(state[14 - j] + ((j == 7)?"":"\t"));
		}
		System.out.println();
	}

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		run(new Scanner(System.in));
	}

}
