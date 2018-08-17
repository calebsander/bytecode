import {parseAndThen, parseRepeated, parseReturn, parseStruct, parseShort} from './parse'
import {Class, ConstantPool} from './constant-pool-parser'

export interface ExceptionTableEntry {
	startPC: number
	endPC: number
	handlerPC: number
	catchType: Class | null
}
export type ExceptionTable = ExceptionTableEntry[]
export const exceptionTableParser = (constantPool: ConstantPool) =>
	parseRepeated(parseStruct<ExceptionTableEntry>([
		['startPC', parseShort],
		['endPC', parseShort],
		['handlerPC', parseShort],
		['catchType', parseAndThen(parseShort, index =>
			parseReturn(index
				? constantPool.getConstant(index) as Class
				: null
			)
		)]
	]))