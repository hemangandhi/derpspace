
/**
 * Class to generate random characters within a bound.
 * Made by 111.
 * Main method only for testing purposes.
 * @author Heman
 *
 */
public class CharUtil {

	public static char randomLetter(){
		return (char)((int) (Math.random() * 6 + (int) 'A'));
	}
	
	public static void main(String[] args){
		for(int i = 0; i < 26; i++)
			System.out.print(randomLetter());
	}
}
