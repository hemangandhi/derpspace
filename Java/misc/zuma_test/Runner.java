/**
 * Main running class to run the game.
 * Made by 111.
 * @author Heman
 *
 */
public class Runner {
	
	public static void run(int speed){
		IO.setUp();
		int pos = 0, score = 0, turns = 0;
		Game g = new Game(15, 3);
		while(!g.gameOver()){
			System.out.print('>');
			for(char i: g.getBalls())
				System.out.print(i);
			for(int i = g.ballCount(); i < 15; i++)
				System.out.print('-');
			System.out.println();
			printTurret(pos, g.getTurret(), 3);
			
			String choice = "";
			do{
				System.out.println("Enter L to move left, R to move right and F to fire!");
				choice = IO.readLine().toUpperCase();
			}while(!choice.equals("L") && !choice.equals("R") && !choice.equals("F"));	
			
			if(choice.equals("L") && pos > 0)
				pos--;
			else if(choice.equals("R") && pos < 50)
				pos++;
			else if(choice.equals("F")){
				score += g.fireTurret(pos);
				continue;
			}
			
			turns++;
			if(turns % speed == 0)
				g.addToBalls();
			
		}
		
		System.out.println("Good game! You got " + score + " points.");
		IO.destroy();
	}
	
	public static void printTurret(int p, char[] v, int turrSize){
		for(int j = 0; j < turrSize; j++){
			for(int i = 0; i < p; i++)
				System.out.print(' ');
			System.out.print("|" + ((v.length >= j + 1)?v[j]:' ') + "|");
			System.out.println();
		}
		System.out.println();
	}

	public static void main(String[] args) {
		run(2);
	}

}
