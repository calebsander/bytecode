import {Parser} from './parse'
import {Attribute} from './attributes-parser'
import {codeParser} from './code-parser'
import {ConstantPool} from './constant-pool-parser'
import {constantValueParser} from './constant-value-parser'
import {FieldOrMethod} from './fields-or-methods-parser'

interface AttributeArgs {
	attribute: Attribute
	className: string
	constantPool: ConstantPool
	method: FieldOrMethod
}
interface ProcessedAttribute {
	type: string
	value: any
}
export function processAttribute({attribute, className, constantPool, method}: AttributeArgs): ProcessedAttribute {
	const type = attribute.type.getValue(constantPool)
	const {info} = attribute
	let parser: Parser<any> | null = null
	switch (type) {
		case 'Code': {
			parser = codeParser({
				constantPool,
				className,
				isStatic: method.accessFlags.static
			})
			break
		}
		case 'ConstantValue': {
			parser = constantValueParser(constantPool)
			break
		}
		default: console.error('Unknown attribute type', type)
	}
	return {type, value: parser && parser(info).result}
}