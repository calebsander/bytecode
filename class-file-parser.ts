import {parseAndThen, parseInt, parseReturn, parseStruct, parseShort} from './parse'
import {AccessFlags, accessFlagsParser} from './access-flags-parser'
import {Attribute, attributesParser} from './attributes-parser'
import {constantParser} from './constant-parser'
import {ConstantPool, Class, constantPoolParser} from './constant-pool-parser'
import {FieldOrMethod, fieldsOrMethodsParser} from './fields-or-methods-parser'
import {interfacesParser} from './interfaces-parser'
import ConstantPoolIndex from './constant-pool-index'

const MAGIC = 0xCAFEBABE

export interface ClassFile {
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
export const classFileParser = parseStruct<ClassFile>([
	['magic', parseAndThen(parseInt, magic => {
		if (magic !== MAGIC) throw new Error('Invalid magic bytes')
		return parseReturn(magic)
	})],
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