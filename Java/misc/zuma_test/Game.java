import dataStructures.*;

/**
 * Class modeling the game and preserving the state.
 * Made by 112.
 * Implementation may vary, only methods labeled "REQ" will be needed (with headers provided). 
 * @author Heman
 *
 */
public class Game {

	private final int LEN;
	
	private BoundedQueue<Character> turret;
	private LinkedList<Character> balls;
	
	/**
	 * Basic constructor.
	 * REQ.
	 * @param len
	 * @param turretCap
	 */
	public Game(int len, int turretCap){
		LEN = len;
		turret = new BoundedQueue<>(turretCap);
		balls = new LinkedList<>();
		setUpTurret(turretCap);
	}
	
	private void setUpTurret(int cap){
		for(int i = 0; i < cap; i++)
			turret.enqueue(CharUtil.randomLetter());
	}
	
	/**
	 * A way to get the turret state.
	 * @return the turret state.
	 * REQ.
	 */
	public char[] getTurret(){
		char[] r = new char[turret.size()];
		Object[] c = turret.vals();
		for(int i = 0; i < r.length; i++)
			r[i] = (char) c[i];
		return r;
	}
	
	/**
	 * Fire the turret.
	 * @param position the position of the turret.
	 * @return the score change due to the firing.
	 * REQ.
	 */
	public int fireTurret(int position){
		char ret = turret.dequeue();
		turret.enqueue(CharUtil.randomLetter());
		balls.add(Math.min(position, balls.size()), ret);
		return balls.removeMatchingAdj(Math.min(position, balls.size() - 1));
	}
	
	/**
	 * Add to the ball list (the potential targets).
	 * REQ.
	 */
	public void addToBalls(){
		balls.addToHead(CharUtil.randomLetter());
	}
	
	/**
	 * 
	 * @return whether the game has ended.
	 * REQ.
	 */
	public boolean gameOver(){
		return balls.size() > LEN;
	}
	
	/**
	 * Get the list of targets.
	 * @return the list of targets.
	 * REQ.
	 */
	public char[] getBalls(){
		char[] r = new char[balls.size()];
		Object[] c = balls.vals();
		for(int i = 0; i < r.length; i++)
			r[i] = (char) c[i];
		return r;
	}
	
	public int ballCount(){
		return balls.size();
	}
	
}
