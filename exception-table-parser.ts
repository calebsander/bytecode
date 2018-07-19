import {Parser, parseAndThen, parseRepeated, parseReturn, parseStruct, parseShort} from './parse'
import ConstantPoolIndex from './constant-pool-index'
import {Class} from './constant-pool-parser'

export interface ExceptionTableEntry {
	startPC: number
	endPC: number
	handlerPC: number
	catchType: ConstantPoolIndex<Class> | null
}
export const exceptionTableParser: Parser<ExceptionTableEntry[]> =
	parseRepeated(parseStruct<ExceptionTableEntry>([
		['startPC', parseShort],
		['endPC', parseShort],
		['handlerPC', parseShort],
		['catchType', parseAndThen(parseShort, index =>
			parseReturn(
				index ? new ConstantPoolIndex(index) : null
			)
		)]
	]))