
public class BracketVal {

	public static final char[][] brackets = {
		{'(',')'},
		{'{','}'},
		{'[',']'},
		{'<','>'}
	};
	
	public static boolean validate(String s){
		PersistentStack<Integer> opens = new PersistentStack<Integer>();
		for(int i = 0; i < s.length(); i++){
			char at = s.charAt(i);
			for(char[] c: brackets){
				if(at == c[0]){
					opens = opens.push(i);
					break;
				}
				if(at == c[1]){
					if(opens.size() == 0 || s.charAt(opens.peek()) != c[0])
						return false;
					else
						opens = opens.pop();
					break;
				}
			}
		}
		return opens.size() == 0;
	}
	
	public static void main(String[] args){
		System.out.println(validate("({[]}(<asdf>){})"));
	}
}
