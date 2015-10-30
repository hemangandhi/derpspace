package misc;

import java.util.Scanner;

public class Min2 {
	
	public static void min2(Scanner sc){
		System.out.println("Enter a terminator:");
		double term = readDouble(sc);
		double min, min2;
		do{
			System.out.println("Enter your first number");
			min2 = readDouble(sc);
			System.out.println("Enter your second number");
			double s = readDouble(sc);
			min = Math.min(s, min2);
			min2 = Math.max(min, s);
			if(min == term || min2 == term)
				System.out.println("Cannot enter terminator for first two terms!");
		}while(min == term || min2 == term);
		
		System.out.println("Now enter all the other numbers on their own line:");
		for(double s = readDouble(sc); s != term; s = readDouble(sc)){
			if(s < min){
				min2 = min;
				min = s;
			}else if(s < min2)
				min2 = s;
		}
		
		System.out.println("The min was " + min);
		System.out.println("The 2nd smallest was " + min2);
	}
	
	public static double readDouble(Scanner sc){
		do{
			try{
				return Double.parseDouble(sc.nextLine());
			}catch (NumberFormatException nfe){
				System.out.println("Enter a double!");
			}
		}while(true);
	}

	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		min2(sc);
		sc.close();

	}

}
