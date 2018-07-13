import {Attribute} from './attributes-parser'
import {codeParser} from './code-parser'
import {ConstantPool} from './constant-pool-parser'
import {FieldOrMethod} from './fields-or-methods-parser'

interface AttributeArgs {
	attribute: Attribute
	constantPool: ConstantPool
	method: FieldOrMethod
}
interface ProcessedAttribute {
	type: string
	value: any
}
export function processAttribute({attribute, constantPool, method}: AttributeArgs): ProcessedAttribute {
	const type = attribute.type.getValue(constantPool)
	const {info: data} = attribute
	let value: any = null
	switch (type) {
		case 'Code': {
			value = codeParser(constantPool, method.accessFlags.static)(new DataView(data)).result
			break
		}
		default: console.error('Unknown attribute type', type)
	}
	return {type, value}
}