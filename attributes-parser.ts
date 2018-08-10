import {parseAndThen, parseByteArray, parseRepeated, parseReturn, parseStruct, parseInt} from './parse'
import {constantParser} from './constant-parser'
import ConstantPoolIndex from './constant-pool-index'

export interface Attribute {
	type: ConstantPoolIndex<string>
	info: DataView
}
export const attributesParser =
	parseRepeated(parseStruct<Attribute>([
		['type', constantParser],
		['info', parseAndThen(parseByteArray(parseInt), bytes =>
			parseReturn(new DataView(bytes))
		)]
	]))