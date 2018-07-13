import {Parser, parseRepeated, parseStruct, parseShort} from './parse'
import {constantParser} from './constant-parser'
import ConstantPoolIndex from './constant-pool-index'
import {Class} from './constant-pool-parser'

export interface ExceptionTable {
	startPC: number
	endPC: number
	handlerPC: number
	catchType: ConstantPoolIndex<Class>
}
export const exceptionTableParser: Parser<ExceptionTable[]> =
	parseRepeated(parseStruct<ExceptionTable>([
		['startPC', parseShort],
		['endPC', parseShort],
		['handlerPC', parseShort],
		['catchType', constantParser]
	]))