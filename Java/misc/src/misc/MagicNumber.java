package misc;

public class MagicNumber {

	public static boolean isPerfect(int n){
		int sum = 0;
		for(int i = 1; i <= n; i++)
			if(n % i == 0)
				sum += i;
		
		return sum / 2 == n;
	}
	
	public static void main(String [] args){
		for(int i = 0, ct = 0; ct < 5; i ++){
			if(isPerfect(i)){
				ct++;
				System.out.println(i);
			}	
		}
	}
}
