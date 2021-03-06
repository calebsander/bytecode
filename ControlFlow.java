import java.util.ArrayList;

class ControlFlow {
	public void whileLoop() {
		boolean b = true;
		int a = 0;
		while (b) {
			a = 1;
			if (b) break;
			a = 2;
			if (b) continue;
			a = 3;
		}
		a = 4;
	}
	public void ifStatement() {
		boolean b = true;
		int a = 0;
		if (b) a = 1;
		a = 2;
	}
	public void ifElse() {
		boolean b = true;
		int a = 0;
		if (b) a = 1;
		else a = 2;
		a = 3;
	}
	public void ifElseIfElse() {
		boolean b = true;
		int a = 0;
		if (b) a = 1;
		else if (!b) a = 2;
		else a = 3;
		a = 4;
	}
	public void doWhile() {
		boolean b = true;
		int a = 0;
		do {
			a = 1;
			if (b) break;
			a = 2;
			if (b) continue;
			a = 3;
		} while (b);
		a = 4;
	}
	public int ternary() {
		int i1 = 1, i2 = 2;
		return i1 - i2 < 0 ? i1 : i2;
	}
	public boolean getFalse() {
		return false;
	}
	public boolean getTrue() {
		return true;
	}
	public boolean getAnd() {
		return getFalse() && getTrue();
	}
	public boolean getOr() {
		return getFalse() || getTrue();
	}
	public int forIn() {
		ArrayList<Integer> list = new ArrayList<>();
		list.add(1);
		list.add(2);
		list.add(3);
		int sum = 0;
		for (int i : list) sum += i;
		for (int i : new int[]{1, 2, 3}) sum += i;
		return sum;
	}
	public void nestedLoops() {
		boolean b = true;
		int a = 0;
		loop1: while (b) {
			a = 1;
			loop2: while (!b) {
				a = 2;
				if (b) break loop1;
				a = 3;
			}
			a = 4;
		}
		loop3: while (b) {
			a = 5;
			loop4: while (!b) {
				a = 6;
				if (b) continue loop4;
				a = 7;
				if (b) continue loop3;
				a = 8;
			}
			a = 9;
		}
		a = 10;
	}
	public boolean multipleAnd() {
		return getTrue() && getTrue() && getFalse();
	}
	public boolean multipleOr() {
		return getFalse() || getTrue() || getFalse();
	}
	public void forLoop() {
		int a = 0;
		for (a = 1; a < 5; a++) {
			a = 2;
			if (a % 2 == 0) break;
			a = 3;
			if (a % 3 == 0) {
				a = 4;
				if (Math.random() > 0.5) continue;
				a = 5; // if this statement is removed, control-flow parsing fails
			}
			a = 6;
		}
		a = 7;
	}
}