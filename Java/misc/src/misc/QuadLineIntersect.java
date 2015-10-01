package misc;

public class QuadLineIntersect {
	
	/***
	 * See if quadratic and line intersect.
	 * @param args
	 */

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		double a = 1;
		double b = 0;
		double c = 0;
		double d = 0;
		double e = 0;
		
		if(a == 0){
			System.out.println("The first curve is a line!");
			if(b == d){
				if(c == e)
					System.out.println("These lines intersect infinitely!");
				else
					System.out.println("These lines are parallel!");
			}else{
				System.out.println("The lines intersect at x = " + (e - c)/(b - d));
			}
		}else{
			double sq = Math.sqrt((b - d)*(b - d) - 4*a*(c - e));
			if(Double.isNaN(sq))
				System.out.println("The curves do not intersect.");
			else{
				double x1 = (d - b + sq)/(2*a), x2 = (d - b - sq)/(2*a);
				System.out.println("The curves intersect at:");
				System.out.println("x = " + x1 + ", y = " + (x1*d + e));
				if(x1 != x2)
					System.out.println("x = " + x2 + ", y = " + (x2*d + e));
			}
		}

	}

}
