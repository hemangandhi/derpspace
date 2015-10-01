package dataStructures;

/**
 * Tester for linked list.
 * For implementation demo only.
 * @author Heman
 *
 */
public class TestLL {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		LinkedList<Integer> ll = new LinkedList<>();
		
		for(int i = 0; i < 10; i++)
			ll.addToHead(i);
		for(int i = 0; i < 5; i++)
			ll.addToHead(9);
		
		System.out.println(ll);
		System.out.println(ll.removeMatchingAdj(9));
		System.out.println(ll);
		System.out.println(ll.size());
		
		for(Object o: ll.vals())
			System.out.print((Integer)o);

	}

}
