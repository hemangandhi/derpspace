package mankala;

public class Game {

	private int[] board;
	private int turn;
	
	public Game(){
		board = new int[14];
		for(int i = 0; i < 6; i++){
			board[i] = 4;
			board[i + 7] = 4;
		}
		board[6] = 0; board[13] = 0;
	}
	
	public void play(int ind){
		if(ind < 0 || ind > 5)
			throw new IllegalArgumentException("Invalid place on board!");
		
		int i = ind + 7*turn;
		if(board[i] == 0)
			return;
		for(int val = board[i]; val > 0; val--, i++){
			board[(i + 1) % board.length]++;
		}
		board[ind + 7*turn] = 0;
		
		if(board[i % board.length] == 1 && i % board.length != 6 && i % board.length != 13){
			board[7*(turn + 1) - 1] += board[i % board.length] + board[(i + 7) % board.length]; 
			board[i % board.length] = board[(i + 7) % board.length] = 0;
		}
		
		int sum = 0;
		for(int j = 7*turn; j < 7*turn + 7; j++){
			sum += board[j % board.length];
		}
		if(sum == 0){
			turn++;
			for(int j = 7*turn; j < 7*turn + 7; j++){
				board[(7*(turn + 1) - 1) % board.length] += board[j % board.length];
				board[j % board.length] = 0;
			}
			turn--;
			return;
		}
		
		if(i % board.length != 6 && i % board.length != 13){
			turn++;
			turn = turn%2;
		}
	}
	
	public int[] scores(){
		return new int[]{board[6],board[13]};
	}
	
	public boolean gameOver(){
		return board[6] > 24 || board[13] > 24 || (board[13] == board[6] && board[6] == 24);
	}
	
	public int winState(){
		if(!gameOver())
			return -2;
		else if(board[13] == board[6])
			return -1;
		else if(board[6] > board[13])
			return 0;
		else
			return 1;
	}
	
	public int[] getBoard(){
		return board;
	}
	
	public int turn(){
		return turn;
	}
}
