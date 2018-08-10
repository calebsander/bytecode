public class FieldTest {
	public static final double CONST = 123e4;

	public static boolean b;
	private static int abc;

	private final String ghi = "abc";
	public Object def;
	private int jkl = -1;

	static {
		b = Math.random() > 0.5;
		abc = 5;
	}

	public FieldTest(Object obj) {
		this.def = obj;
	}
	public FieldTest() {
		this(new Object());
	}

	public int computeSomething() {
		if (b) return -abc;
		return abc ^ this.def.hashCode() ^ this.ghi.hashCode() ^ jkl;
	}
}