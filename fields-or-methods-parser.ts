import {Parser, parseRepeated, parseStruct} from './parse'
import {AccessFlags, accessFlagsParser} from './access-flags-parser'
import {constantParser} from './constant-parser'
import ConstantPoolIndex from './constant-pool-index'
import {Attribute, attributesParser} from './attributes-parser'

export interface FieldOrMethod {
	accessFlags: AccessFlags
	name: ConstantPoolIndex<string>
	descriptor: ConstantPoolIndex<string>
	attributes: Attribute[]
}
export const fieldsOrMethodsParser: Parser<FieldOrMethod[]> =
	parseRepeated(parseStruct<FieldOrMethod>([
		['accessFlags', accessFlagsParser],
		['name', constantParser],
		['descriptor', constantParser],
		['attributes', attributesParser]
	]))