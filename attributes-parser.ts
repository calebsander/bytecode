import {Parser, parseByteArray, parseRepeated, parseStruct, parseInt} from './parse'
import {constantParser} from './constant-parser'
import ConstantPoolIndex from './constant-pool-index'

export interface Attribute {
	type: ConstantPoolIndex<string>
	info: ArrayBuffer
}
export const attributesParser: Parser<Attribute[]> =
	parseRepeated(parseStruct<Attribute>([
		['type', constantParser],
		['info', parseByteArray(parseInt)]
	]))