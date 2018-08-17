import {parseStruct, parseShort} from './parse'
import {Attribute, attributesParser} from './attributes-parser'
import {bytecodeParser, Code, MethodContext} from './bytecode-parser'
import {ExceptionTable, exceptionTableParser} from './exception-table-parser'

export interface CodeAttribute {
	maxStack: number
	maxLocals: number
	instructions: Code
	exceptionTable: ExceptionTable
	attributes: Attribute[]
}

export const codeParser = (context: MethodContext) =>
	parseStruct<CodeAttribute>([
		['maxStack', parseShort],
		['maxLocals', parseShort],
		['instructions', bytecodeParser(context)],
		['exceptionTable', exceptionTableParser(context.constantPool)],
		['attributes', attributesParser]
	])