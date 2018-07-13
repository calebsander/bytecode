import {
	Parser,
	ParseResult,
	parseAndThen,
	parseReturn,
	parseByteArray,
	parseByte,
	parseShort,
	parseInt,
	slice
} from './parse'
import utf8Decode from './utf8-decode'

const CONSTANT_Class = 7
const CONSTANT_Fieldref = 9
const CONSTANT_Methodref = 10
const CONSTANT_InterfaceMethodref = 11
const CONSTANT_String = 8
const CONSTANT_Integer = 3
const CONSTANT_Float = 4
const CONSTANT_Long = 5
const CONSTANT_Double = 6
const CONSTANT_NameAndType = 12
const CONSTANT_Utf8 = 1
const CONSTANT_MethodHandle = 15
const CONSTANT_MethodType = 16
const CONSTANT_InvokeDynamic = 18

interface PoolValueStruct {
	[key: string]: PoolValue<any>
}
export class ConstantPool {
	private readonly pool: (ConstantValue<any> | PoolValueStruct | undefined)[]

	constructor(numConstants: number) {
		this.pool = new Array(numConstants)
	}

	public setConstant(index: number, value: ConstantValue<any> | PoolValueStruct) {
		this.pool[index] = value
	}
	public getConstant(index: number): any {
		const rawValue = this.pool[index]
		if (rawValue === undefined) throw new Error('No constant at index ' + String(index))
		if (rawValue instanceof ConstantValue) return rawValue.getValue()
		//Otherwise, must be a mapping of strings to PoolValues
		const value: any = {}
		for (const key in rawValue) {
			if ({}.hasOwnProperty.call(rawValue, key)) value[key] = rawValue[key].getValue()
		}
		return value
	}
}
class PoolValue<E> {
	private value: E | undefined

	constructor(private readonly pool: ConstantPool, private readonly index: number) {}

	getValue(): E {
		if (this.value === undefined) this.value = this.pool.getConstant(this.index)
		return this.value!
	}
}
class ConstantValue<E> {
	constructor(private readonly value: E) {}

	public getValue(): E {
		return this.value
	}
}

interface EntryCount {
	entries: number
}
type PoolValueParser<E> = (pool: ConstantPool) => (data: DataView) =>
	ParseResult<E> & EntryCount

type ReferencedValue<E> = {
	[key in keyof E]: PoolValue<E[key]> | ConstantValue<E[key]>
}
export interface Ref {
	class: Class
	nameAndType: NameAndType
}
const refDataParser = parseAndThen(parseShort, classIndex =>
	parseAndThen(parseShort, nameAndTypeIndex =>
		parseReturn({classIndex, nameAndTypeIndex})
	)
)
const REF_PARSER: PoolValueParser<ReferencedValue<Ref>> = pool => data => {
	const {result: {classIndex, nameAndTypeIndex}, length} = refDataParser(data)
	return {
		result: {
			class: new PoolValue(pool, classIndex),
			nameAndType: new PoolValue(pool, nameAndTypeIndex)
		},
		length,
		entries: 1
	}
}
export interface Class {
	name: string
}
const CLASS_PARSER: PoolValueParser<ReferencedValue<Class>> = pool => data => {
	const {result: nameIndex, length} = parseShort(data)
	return {
		result: {
			name: new PoolValue(pool, nameIndex)
		},
		length,
		entries: 1
	}
}
export type LiteralConstant
	= {type: 'string', value: string}
	| {type: 'int' | 'float' | 'double', value: number}
	| {type: 'long', value: BigInt}
const STRING_PARSER: PoolValueParser<ReferencedValue<LiteralConstant>> = pool => data => {
	const {result: utf8Index, length} = parseShort(data)
	return {
		result: {
			type: new ConstantValue<'string'>('string'),
			value: new PoolValue<string>(pool, utf8Index)
		},
		length,
		entries: 1
	}
}
const parseSignedInt = parseAndThen(parseInt, i => parseReturn(i | 0))
const INTEGER_PARSER: PoolValueParser<ReferencedValue<LiteralConstant>> = _ => data => {
	const {result, length} = parseSignedInt(data)
	return {
		result: {
			type: new ConstantValue<'int'>('int'),
			value: new ConstantValue(result)
		},
		length,
		entries: 1
	}
}
const FLOAT_PARSER: PoolValueParser<ReferencedValue<LiteralConstant>> = _ => data => ({
	result: {
		type: new ConstantValue<'float'>('float'),
		value: new ConstantValue(data.getFloat32(0))
	},
	length: 4,
	entries: 1
})
const longParser = parseAndThen(parseSignedInt, upper =>
	parseAndThen(parseSignedInt, lower =>
		parseReturn({upper, lower})
	)
)
const LONG_PARSER: PoolValueParser<ReferencedValue<LiteralConstant>> = _ => data => {
	const {result: {upper, lower}, length} = longParser(data)
	return {
		result: {
			type: new ConstantValue<'long'>('long'),
			value: new ConstantValue(BigInt(upper) << BigInt(32) | BigInt(lower))
		},
		length,
		entries: 2
	}
}
const DOUBLE_PARSER: PoolValueParser<ReferencedValue<LiteralConstant>> = _ => data => ({
	result: {
		type: new ConstantValue<'double'>('double'),
		value: new ConstantValue(data.getFloat64(0))
	},
	length: 8,
	entries: 2
})
export interface NameAndType {
	name: string
	descriptor: string
}
const nameAndTypeDataParser = parseAndThen(parseShort, nameIndex =>
	parseAndThen(parseShort, descriptorIndex =>
		parseReturn({nameIndex, descriptorIndex})
	)
)
const NAME_AND_TYPE_PARSER: PoolValueParser<ReferencedValue<NameAndType>> = pool => data => {
	const {result: {nameIndex, descriptorIndex}, length} = nameAndTypeDataParser(data)
	return {
		result: {
			name: new PoolValue(pool, nameIndex),
			descriptor: new PoolValue(pool, descriptorIndex)
		},
		length,
		entries: 1
	}
}
const utf8DataParser = parseByteArray(parseShort)
const UTF8_PARSER: PoolValueParser<ConstantValue<string>> = _ => data => {
	const {result, length} = utf8DataParser(data)
	return {
		result: new ConstantValue(utf8Decode(new Uint8Array(result))),
		length,
		entries: 1
	}
}
interface MethodHandle {
	referenceKind: number
	reference: Ref
}
const methodHandleDataParser = parseAndThen(parseByte, kind =>
	parseAndThen(parseShort, referenceIndex =>
		parseReturn({kind, referenceIndex})
	)
)
const METHOD_HANDLE_PARSER: PoolValueParser<ReferencedValue<MethodHandle>> = pool => data => {
	const {result: {kind, referenceIndex}, length} = methodHandleDataParser(data)
	return {
		result: {
			referenceKind: new ConstantValue(kind),
			reference: new PoolValue<Ref>(pool, referenceIndex)
		},
		length,
		entries: 1
	}
}
interface MethodType {
	descriptor: string
}
const METHOD_TYPE_PARSER: PoolValueParser<ReferencedValue<MethodType>> = pool => data => {
	const {result: descriptorIndex, length} = parseShort(data)
	return {
		result: {
			descriptor: new PoolValue(pool, descriptorIndex)
		},
		length,
		entries: 1
	}
}
interface InvokeDynamic {
	bootstrapMethodAttrIndex: number
	nameAndType: NameAndType
}
const invokeDynamicDataParser = parseAndThen(parseShort, bootstrap =>
	parseAndThen(parseShort, nameAndTypeIndex =>
		parseReturn({bootstrap, nameAndTypeIndex})
	)
)
const INVOKE_DYNAMIC_PARSER: PoolValueParser<ReferencedValue<InvokeDynamic>> = pool => data => {
	const {result: {bootstrap, nameAndTypeIndex}, length} = invokeDynamicDataParser(data)
	return {
		result: {
			bootstrapMethodAttrIndex: new ConstantValue(bootstrap),
			nameAndType: new PoolValue(pool, nameAndTypeIndex)
		},
		length,
		entries: 1
	}
}
const CONSTANT_PARSERS = new Map<number, PoolValueParser<ConstantValue<any> | {}>>()
	.set(CONSTANT_Class, CLASS_PARSER)
	.set(CONSTANT_Fieldref, REF_PARSER)
	.set(CONSTANT_Methodref, REF_PARSER)
	.set(CONSTANT_InterfaceMethodref, REF_PARSER)
	.set(CONSTANT_String, STRING_PARSER)
	.set(CONSTANT_Integer, INTEGER_PARSER)
	.set(CONSTANT_Float, FLOAT_PARSER)
	.set(CONSTANT_Long, LONG_PARSER)
	.set(CONSTANT_Double, DOUBLE_PARSER)
	.set(CONSTANT_NameAndType, NAME_AND_TYPE_PARSER)
	.set(CONSTANT_Utf8, UTF8_PARSER)
	.set(CONSTANT_MethodHandle, METHOD_HANDLE_PARSER)
	.set(CONSTANT_MethodType, METHOD_TYPE_PARSER)
	.set(CONSTANT_InvokeDynamic, INVOKE_DYNAMIC_PARSER)

export const constantPoolParser: Parser<ConstantPool> =
	parseAndThen(parseShort, numConstants =>
		data => {
			let length = 0
			const pool = new ConstantPool(numConstants)
			for (let constantIndex = 1; constantIndex < numConstants;) {
				const constantTag = data.getUint8(length)
				length++
				const constantParser = CONSTANT_PARSERS.get(constantTag)
				if (!constantParser) throw new Error('Unknown constant pool type: ' + String(constantTag))
				const {result, length: valueLength, entries} = constantParser(pool)(slice(data, length))
				pool.setConstant(constantIndex, result)
				constantIndex += entries
				length += valueLength
			}
			return {
				result: pool,
				length
			}
		}
	)