import java.util.ArrayList;
import java.util.Iterator;

public class ConstantPoolTest implements Iterable<Integer> {
	public static void main(String[] args) {
		System.out.println(12345L);
		System.out.println(123.0);
		System.out.println(123F);
		System.out.println(12346789);
		System.out.println("abc");
		Iterator<Integer> i = null;
		System.out.println(i.hasNext());
		i.hashCode();
	}

	private ArrayList<Integer> list;
	public static int abc;
	public ConstantPoolTest() {
		this.list = new ArrayList<>();
		list.add(1);
		list.add(2);
		list.add(3);
	}
	public Iterator<Integer> iterator() {
		return this.list.iterator();
	}
}