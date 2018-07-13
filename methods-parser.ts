import {Parser, parseRepeated, parseStruct} from './parse'
import {AccessFlags, accessFlagsParser} from './access-flags-parser'
import {Attribute, attributesParser} from './attributes-parser'
import {constantParser} from './constant-parser'
import ConstantPoolIndex from './constant-pool-index'

export interface Method {
	accessFlags: AccessFlags
	name: ConstantPoolIndex<string>
	descriptor: ConstantPoolIndex<string>
	attributes: Attribute[]
}

export const methodsParser: Parser<Method[]> =
	parseRepeated(parseStruct<Method>([
		['accessFlags', accessFlagsParser],
		['name', constantParser],
		['descriptor', constantParser],
		['attributes', attributesParser]
	]))