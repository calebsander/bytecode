public enum EnumTest {
	ABC(1),
	DEF(2);

	public final int param;

	EnumTest(int param) {
		this.param = param;
	}

	public EnumTest getOther() {
		switch (this) {
			case ABC:
				return DEF;
			case DEF:
				return ABC;
			default:
				return null;
		}
	}
}