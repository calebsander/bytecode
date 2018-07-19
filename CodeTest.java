import java.util.ArrayList;
import java.util.List;

public class CodeTest {
	static int staticInt;
	List<Double> instanceList = new ArrayList<>();

	public CodeTest t() {
		return this;
	}
	public int add(int i1, int i2) {
		return i1 + i2;
	}
	public void tryCatch() {
		try {
			throw new RuntimeException();
		}
		catch (RuntimeException e) {}
	}
	public static void copyInts(int a, int b, int c, int d, int e) {
		a = b;
		b = c;
		c = d;
		d = e;
		e = a;
	}
	public static void copyObjects(Object a, Object b, Object c, Object d, Object e) {
		a = b;
		b = c;
		c = d;
		d = e;
		e = a;
	}
	public int getLength(Object[] arr) {
		return arr.length;
	}
	public Object nullObject() {
		return null;
	}
	public int intConsts() {
		int a = -1, b = 0, c = 1, d = 2, e = 3, f = 4, g = 5;
		int h = 0;
		h *= new Integer("1"); h *= new Integer("2"); h *= new Integer("3"); h *= new Integer("4"); h *= new Integer("5"); h *= new Integer("6"); h *= new Integer("7"); h *= new Integer("8"); h *= new Integer("9"); h *= new Integer("10"); h *= new Integer("11"); h *= new Integer("12"); h *= new Integer("13"); h *= new Integer("14"); h *= new Integer("15"); h *= new Integer("16"); h *= new Integer("17"); h *= new Integer("18"); h *= new Integer("19"); h *= new Integer("20"); h *= new Integer("21"); h *= new Integer("22"); h *= new Integer("23"); h *= new Integer("24"); h *= new Integer("25"); h *= new Integer("26"); h *= new Integer("27"); h *= new Integer("28"); h *= new Integer("29"); h *= new Integer("30"); h *= new Integer("31"); h *= new Integer("32"); h *= new Integer("33"); h *= new Integer("34"); h *= new Integer("35"); h *= new Integer("36"); h *= new Integer("37"); h *= new Integer("38"); h *= new Integer("39"); h *= new Integer("40"); h *= new Integer("41"); h *= new Integer("42"); h *= new Integer("43"); h *= new Integer("44"); h *= new Integer("45"); h *= new Integer("46"); h *= new Integer("47"); h *= new Integer("48"); h *= new Integer("49"); h *= new Integer("50"); h *= new Integer("51"); h *= new Integer("52"); h *= new Integer("53"); h *= new Integer("54"); h *= new Integer("55"); h *= new Integer("56"); h *= new Integer("57"); h *= new Integer("58"); h *= new Integer("59"); h *= new Integer("60"); h *= new Integer("61"); h *= new Integer("62"); h *= new Integer("63"); h *= new Integer("64"); h *= new Integer("65"); h *= new Integer("66"); h *= new Integer("67"); h *= new Integer("68"); h *= new Integer("69"); h *= new Integer("70"); h *= new Integer("71"); h *= new Integer("72"); h *= new Integer("73"); h *= new Integer("74"); h *= new Integer("75"); h *= new Integer("76"); h *= new Integer("77"); h *= new Integer("78"); h *= new Integer("79"); h *= new Integer("80"); h *= new Integer("81"); h *= new Integer("82"); h *= new Integer("83"); h *= new Integer("84"); h *= new Integer("85"); h *= new Integer("86"); h *= new Integer("87"); h *= new Integer("88"); h *= new Integer("89"); h *= new Integer("90"); h *= new Integer("91"); h *= new Integer("92"); h *= new Integer("93"); h *= new Integer("94"); h *= new Integer("95"); h *= new Integer("96"); h *= new Integer("97"); h *= new Integer("98"); h *= new Integer("99"); h *= new Integer("100"); h *= new Integer("101"); h *= new Integer("102"); h *= new Integer("103"); h *= new Integer("104"); h *= new Integer("105"); h *= new Integer("106"); h *= new Integer("107"); h *= new Integer("108"); h *= new Integer("109"); h *= new Integer("110"); h *= new Integer("111"); h *= new Integer("112"); h *= new Integer("113"); h *= new Integer("114"); h *= new Integer("115"); h *= new Integer("116"); h *= new Integer("117"); h *= new Integer("118"); h *= new Integer("119"); h *= new Integer("120"); h *= new Integer("121"); h *= new Integer("122"); h *= new Integer("123"); h *= new Integer("124"); h *= new Integer("125"); h *= new Integer("126"); h *= new Integer("127"); h *= new Integer("128"); h *= new Integer("129"); h *= new Integer("130"); h *= new Integer("131"); h *= new Integer("132"); h *= new Integer("133"); h *= new Integer("134"); h *= new Integer("135"); h *= new Integer("136"); h *= new Integer("137"); h *= new Integer("138"); h *= new Integer("139"); h *= new Integer("140"); h *= new Integer("141"); h *= new Integer("142"); h *= new Integer("143"); h *= new Integer("144"); h *= new Integer("145"); h *= new Integer("146"); h *= new Integer("147"); h *= new Integer("148"); h *= new Integer("149"); h *= new Integer("150"); h *= new Integer("151"); h *= new Integer("152"); h *= new Integer("153"); h *= new Integer("154"); h *= new Integer("155"); h *= new Integer("156"); h *= new Integer("157"); h *= new Integer("158"); h *= new Integer("159"); h *= new Integer("160"); h *= new Integer("161"); h *= new Integer("162"); h *= new Integer("163"); h *= new Integer("164"); h *= new Integer("165"); h *= new Integer("166"); h *= new Integer("167"); h *= new Integer("168"); h *= new Integer("169"); h *= new Integer("170"); h *= new Integer("171"); h *= new Integer("172"); h *= new Integer("173"); h *= new Integer("174"); h *= new Integer("175"); h *= new Integer("176"); h *= new Integer("177"); h *= new Integer("178"); h *= new Integer("179"); h *= new Integer("180"); h *= new Integer("181"); h *= new Integer("182"); h *= new Integer("183"); h *= new Integer("184"); h *= new Integer("185"); h *= new Integer("186"); h *= new Integer("187"); h *= new Integer("188"); h *= new Integer("189"); h *= new Integer("190"); h *= new Integer("191"); h *= new Integer("192"); h *= new Integer("193"); h *= new Integer("194"); h *= new Integer("195"); h *= new Integer("196"); h *= new Integer("197"); h *= new Integer("198"); h *= new Integer("199"); h *= new Integer("200"); h *= new Integer("201"); h *= new Integer("202"); h *= new Integer("203"); h *= new Integer("204"); h *= new Integer("205"); h *= new Integer("206"); h *= new Integer("207"); h *= new Integer("208"); h *= new Integer("209"); h *= new Integer("210"); h *= new Integer("211"); h *= new Integer("212"); h *= new Integer("213"); h *= new Integer("214"); h *= new Integer("215"); h *= new Integer("216"); h *= new Integer("217"); h *= new Integer("218"); h *= new Integer("219"); h *= new Integer("220"); h *= new Integer("221"); h *= new Integer("222"); h *= new Integer("223"); h *= new Integer("224"); h *= new Integer("225"); h *= new Integer("226"); h *= new Integer("227"); h *= new Integer("228"); h *= new Integer("229"); h *= new Integer("230"); h *= new Integer("231"); h *= new Integer("232"); h *= new Integer("233"); h *= new Integer("234"); h *= new Integer("235"); h *= new Integer("236"); h *= new Integer("237"); h *= new Integer("238"); h *= new Integer("239"); h *= new Integer("240"); h *= new Integer("241"); h *= new Integer("242"); h *= new Integer("243"); h *= new Integer("244"); h *= new Integer("245"); h *= new Integer("246"); h *= new Integer("247"); h *= new Integer("248"); h *= new Integer("249"); h *= new Integer("250"); h *= new Integer("251"); h *= new Integer("252"); h *= new Integer("253"); h *= new Integer("254"); h *= new Integer("255"); h *= new Integer("256"); h *= new Integer("257"); h *= new Integer("258"); h *= new Integer("259"); h *= new Integer("260"); h *= new Integer("261"); h *= new Integer("262"); h *= new Integer("263"); h *= new Integer("264"); h *= new Integer("265"); h *= new Integer("266"); h *= new Integer("267"); h *= new Integer("268"); h *= new Integer("269"); h *= new Integer("270"); h *= new Integer("271"); h *= new Integer("272"); h *= new Integer("273"); h *= new Integer("274"); h *= new Integer("275"); h *= new Integer("276"); h *= new Integer("277"); h *= new Integer("278"); h *= new Integer("279"); h *= new Integer("280"); h *= new Integer("281"); h *= new Integer("282"); h *= new Integer("283"); h *= new Integer("284"); h *= new Integer("285"); h *= new Integer("286"); h *= new Integer("287"); h *= new Integer("288"); h *= new Integer("289"); h *= new Integer("290"); h *= new Integer("291"); h *= new Integer("292"); h *= new Integer("293"); h *= new Integer("294"); h *= new Integer("295"); h *= new Integer("296"); h *= new Integer("297"); h *= new Integer("298"); h *= new Integer("299"); h *= new Integer("300");
		h++;
		h += 3;
		return 14;
	}
	public static long staticLongConsts() {
		long a = 0, b = 1;
		return a + b;
	}
	public long longConsts() {
		long a = 0, b = 1, c = 2;
		return a + b + c;
	}
	public static float floatConsts() {
		float a = 0, b = 1, c = 2, d = 3, e = 4;
		return a + b + c + d + e;
	}
	public static double staticDoubleConsts() {
		double a = 0, b = 1;
		return a + b;
	}
	public double doubleConsts() {
		double a = 0, b = 1, c = 2;
		return a + b + c;
	}
	public short shortConsts() {
		short a = -257;
		return a;
	}
	public void arrayAccesses() {
		int[] a = new int[1];
		a[0] = a[0];
		long[] b = new long[1];
		b[0] = b[0];
		float[] c = new float[1];
		c[0] = c[0];
		double[] d = new double[1];
		d[0] = d[0];
		Object[] e = new Object[1];
		e[0] = e[0];
		byte[] f = new byte[1];
		f[0] = f[0];
		char[] g = new char[1];
		g[0] = g[0];
		short[] h = new short[1];
		h[0] = h[0];
	}
	public void popTest() {
		ArrayList<Integer> a = new ArrayList<>();
		a.add(1);
		new Long(1).longValue();
	}
	public void sub() {
		int i1 = 1, i2 = 2, i3 = i1 - i2;
		long l1 = 1, l2 = 2, l3 = l1 - l2;
		float f1 = 1, f2 = 2, f3 = f1 - f2;
		double d1 = 1, d2 = 2, d3 = d1 - d2;
	}
	public void mul() {
		int i1 = 1, i2 = 2, i3 = i1 * i2;
		long l1 = 1, l2 = 2, l3 = l1 * l2;
		float f1 = 1, f2 = 2, f3 = f1 * f2;
		double d1 = 1, d2 = 2, d3 = d1 * d2;
	}
	public void div() {
		int i1 = 1, i2 = 2, i3 = i1 / i2;
		long l1 = 1, l2 = 2, l3 = l1 / l2;
		float f1 = 1, f2 = 2, f3 = f1 / f2;
		double d1 = 1, d2 = 2, d3 = d1 / d2;
	}
	public void rem() {
		int i1 = 1, i2 = 2, i3 = i1 % i2;
		long l1 = 1, l2 = 2, l3 = l1 % l2;
		float f1 = 1, f2 = 2, f3 = f1 % f2;
		double d1 = 1, d2 = 2, d3 = d1 % d2;
	}
	public void neg() {
		int i1 = 1, i2 = -i1;
		long l1 = 1, l2 = -l1;
		float f1 = 1, f2 = -f1;
		double d1 = 1, d2 = -d1;
	}
	public void shifts() {
		int  i1 = -1, i2 = i1 << 2, i3 = i1 >> 2, i4 = i1 >>> 2;
		long l1 = -1, l2 = l1 << 2, l3 = l1 >> 2, l4 = l1 >>> 2;
	}
	public void bitwise() {
		int  i1 = 5, i2 = -5, i3 = i1 & i2, i4 = i1 | i2, i5 = i1 ^ i2;
		long l1 = 5, l2 = -5, l3 = l1 & l2, l4 = l1 | l2, l5 = l1 ^ l2;
	}
	public void primitiveCasts() {
		int i = 1;
		long l = 2;
		float f = 3;
		double d = 4;
		i = (int)l; i = (int)f; i = (int)d;
		l = i; l = (long)f; l = (long)d;
		f = i; f = l; f = (float)d;
		d = i; d = l; d = f;
		byte b = 5;
		char c = '6';
		short s = 7;
		b = (byte)i; b = (byte)l; b = (byte)f; b = (byte)d;
		c = (char)i; c = (char)l; c = (char)f; c = (char)d;
		s = (short)i; s = (short)l; s = (short)f; s = (short)d;
	}
	public long longCmp() {
		long l1 = 1, l2 = 2;
		return l1 < l2 ? l1 : l2;
	}
	public float floatCmp(float f) {
		if (f < 0) return -1;
		if (f > 0) return 1;
		return 0;
	}
	public double doubleCmp(double d) {
		if (d < 0) return -1;
		if (d > 0) return 1;
		return 0;
	}
	public void ifConditions() {
		boolean b = true;
		if (b) b = false;
		if (!b) b = true;
		int a = -10;
		do { a++; } while (a < 0);
		if (a <= 0) a *= 10;
	}
	public void intIfConditions() {
		int a = 3;
		if (a != 3) a = 5;
		if (a == 10) a = 4;
		if (a >= 5) a = 2;
		if (a < 5) a = 7;
		if (a <= 8) a = 9;
		if (a > 100) a = 0;
	}
	public void objectComparison() {
		Object a = new Object();
		if (a != a) a = a.getClass();
		if (a == a) a = a.getClass();
	}
	public int switchStatement() {
		int a = 1;
		switch (a * 2) {
			case -1:
				a = 1;
				break;
			case 1:
			case 2:
				a = 2;
				break;
			case 5:
				a = 3;
			default:
				a = 4;
		}
		return a;
	}
	public int stringSwitch() {
		int result = 0;
		switch (this.getClass().getName().hashCode()) {
			case -803337153: //"CodeTest".hashCode()
				result = 1;
			case -1939501217: //"Object".hashCode()
				result++;
				break;
			default:
				result = 3;
		}
		return result;
	}
	public double staticField() {
		staticInt = 100;
		return staticInt;
	}
	public int instanceField() {
		double newItem = Math.random() * 100;
		this.instanceList.add(newItem);
		return this.instanceList.size();
	}
}