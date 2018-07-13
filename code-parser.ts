import {Parser, parseStruct, parseShort} from './parse'
import {Attribute, attributesParser} from './attributes-parser'
import {bytecodeParser, Operation} from './bytecode-parser'
import {ConstantPool} from './constant-pool-parser'
import {ExceptionTable, exceptionTableParser} from './exception-table-parser'

interface Code {
	maxStack: number
	maxLocals: number
	instructions: Operation[]
	exceptionTable: ExceptionTable
	attributes: Attribute[]
}

export const codeParser: (constantPool: ConstantPool, isStatic: boolean) => Parser<Code> =
	(constantPool, isStatic) =>
		parseStruct<Code>([
			['maxStack', parseShort],
			['maxLocals', parseShort],
			['instructions', bytecodeParser(constantPool, isStatic)],
			['exceptionTable', exceptionTableParser],
			['attributes', attributesParser]
		])