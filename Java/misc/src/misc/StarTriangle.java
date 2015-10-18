package misc;

public class StarTriangle {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		starTriangle(6);

	}
	
	public static void starTriangle(int n){
		for(String s = "*"; s.length() <= n; s += '*')
			System.out.println(s);
	}

}
