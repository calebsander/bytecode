// import {inspect} from 'util'
import {Parser, parseAndThen, parseStruct, parseShort, parseInt, parseReturn} from './parse'
import {AccessFlags, accessFlagsParser} from './access-flags-parser'
import {blockToString} from './ast'
import {Attribute, attributesParser} from './attributes-parser'
import {Code} from './bytecode-parser'
import {cleanup} from './cleanup-ast'
import {constantParser} from './constant-parser'
import {ConstantPool, Class, constantPoolParser} from './constant-pool-parser'
import {parseControlFlow} from './control-flow'
import {FieldOrMethod, fieldsOrMethodsParser} from './fields-or-methods-parser'
import {interfacesParser} from './interfaces-parser'
import ConstantPoolIndex from './constant-pool-index'
import {processAttribute} from './process-attribute'

interface ClassFile {
	magic: number
	minorVersion: number
	majorVersion: number
	constantPool: ConstantPool
	accessFlags: AccessFlags
	thisClass: ConstantPoolIndex<Class>
	superClass: ConstantPoolIndex<Class>
	interfaces: ConstantPoolIndex<Class>[]
	fields: FieldOrMethod[]
	methods: FieldOrMethod[]
	attributes: Attribute[]
}
const parseRaw = parseStruct<ClassFile>([
	['magic', parseInt],
	['minorVersion', parseShort],
	['majorVersion', parseShort],
	['constantPool', constantPoolParser],
	['accessFlags', accessFlagsParser],
	['thisClass', constantParser],
	['superClass', constantParser],
	['interfaces', interfacesParser],
	['fields', fieldsOrMethodsParser],
	['methods', fieldsOrMethodsParser],
	['attributes', attributesParser],
])
export const classFileParser: Parser<ClassFile> =
	parseAndThen(parseRaw, result => {
		const {constantPool} = result
		for (const method of result.methods) {
			for (const attribute of method.attributes) {
				const parsedAttribute = processAttribute({attribute, constantPool, method})
				if (parsedAttribute.type === 'Code') {
					const instructions = parsedAttribute.value.instructions as Code
					console.log(method.name.getValue(constantPool), method.descriptor.getValue(constantPool))
					if (instructions.size < 100) console.log(instructions /*inspect(instructions, false, Infinity)*/)
					const block = parseControlFlow(instructions)
					//console.log(blockToString(block))
					const cleanedBlock = cleanup(block)
					console.log(blockToString(cleanedBlock))
					// console.log(inspect(cleanup(parseControlFlow(instructions)), false, Infinity))
					console.log()
				}
			}
		}
		return parseReturn(result)
	})