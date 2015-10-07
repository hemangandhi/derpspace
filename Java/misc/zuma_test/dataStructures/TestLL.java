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
			ll.add(ll.size(), i);
		for(int i = 0; i < 5; i++)
			ll.add(ll.size(), 9);
		
		System.out.println(ll);
		System.out.println(ll.removeMatchingAdj(ll.size() - 1));
		System.out.println(ll);
		System.out.println(ll.size());
	}

}
