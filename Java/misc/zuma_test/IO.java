import java.util.Scanner;

/**
 * Class to model the CS 111 IO class' required functionality for this project.
 * @author Heman
 *
 */
public class IO {
	
	private static Scanner in;
	
	public static void setUp(){
		in = new Scanner(System.in);
	}

	public static String readLine(){
		String ret = in.nextLine();
		return ret;
	}
	
	public static void destroy(){
		in.close();
	}
}
