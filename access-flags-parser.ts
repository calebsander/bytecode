import {Parser, parseAndThen, parseReturn, parseShort} from './parse'

const ACCESS_FLAGS = {
	public:     0x0001,
	private:    0x0002,
	protected:  0x0004,
	static:     0x0008,
	final:      0x0010,
	super:      0x0020, synchronized: 0x0020,
	volatile:   0x0040, bridge:       0x0040,
	transient:  0x0080, varargs:      0x0080,
	native:     0x0100,
	interface:  0x0200,
	abstract:   0x0400,
	strict:     0x0800,
	synthetic:  0x1000,
	annotation: 0x2000,
	enum:       0x4000
}
type AccessFlagsParameterized<Flags> = {
	[flag in keyof Flags]: boolean
}
export type AccessFlags = AccessFlagsParameterized<typeof ACCESS_FLAGS>
interface Indexable<E> {
	[flag: string]: E
}
export const accessFlagsParser: Parser<AccessFlags> =
	parseAndThen(parseShort, flagShort => {
		const flags = {} as AccessFlags
		for (const flag in ACCESS_FLAGS) {
			(flags as Indexable<boolean>)[flag] = !!(flagShort & (ACCESS_FLAGS as Indexable<number>)[flag])
		}
		return parseReturn(flags)
	})