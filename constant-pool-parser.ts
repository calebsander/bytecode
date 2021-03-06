import {
	Parser,
	ParseResult,
	parseAndThen,
	parseReturn,
	parseByteArray,
	parseByte,
	parseSignedInt,
	parseShort,
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
	[key: string]: PoolValue<any> | ConstantValue<any>
}
export class ConstantPool {
	private readonly pool: (ConstantValue<any> | PoolValueStruct | undefined)[]

	constructor(numConstants: number) {
		this.pool = new Array(numConstants)
	}

	setConstant(index: number, value: ConstantValue<any> | PoolValueStruct) {
		this.pool[index] = value
	}
	getConstant(index: number): any {
		const rawValue = this.pool[index]
		if (rawValue === undefined) throw new Error(`No constant at index ${index}`)
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

	constructor(
		private readonly pool: ConstantPool,
		private readonly index: number
	) {}

	getValue(): E {
		if (this.value === undefined) this.value = this.pool.getConstant(this.index)
		return this.value!
	}
}
class ConstantValue<E> {
	constructor(private readonly value: E) {}

	getValue(): E {
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
export type LiteralConstant
	= {type: 'class', name: string}
	| {type: 'string', value: string}
	| {type: 'int' | 'float' | 'double', value: number}
	| {type: 'long', value: bigint}
const CLASS_PARSER: PoolValueParser<ReferencedValue<Class & LiteralConstant>> = pool => data => {
	const {result: nameIndex, length} = parseShort(data)
	return {
		result: {
			type: new ConstantValue<'class'>('class'),
			name: new PoolValue(pool, nameIndex)
		},
		length,
		entries: 1
	}
}
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
		parseReturn(BigInt(upper) << 32n | BigInt(lower))
	)
)
const LONG_PARSER: PoolValueParser<ReferencedValue<LiteralConstant>> = _ => data => {
	const {result, length} = longParser(data)
	return {
		result: {
			type: new ConstantValue<'long'>('long'),
			value: new ConstantValue(result)
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
const CONSTANT_PARSERS = new Map<number, PoolValueParser<any>>([
	[CONSTANT_Class, CLASS_PARSER],
	[CONSTANT_Fieldref, REF_PARSER],
	[CONSTANT_Methodref, REF_PARSER],
	[CONSTANT_InterfaceMethodref, REF_PARSER],
	[CONSTANT_String, STRING_PARSER],
	[CONSTANT_Integer, INTEGER_PARSER],
	[CONSTANT_Float, FLOAT_PARSER],
	[CONSTANT_Long, LONG_PARSER],
	[CONSTANT_Double, DOUBLE_PARSER],
	[CONSTANT_NameAndType, NAME_AND_TYPE_PARSER],
	[CONSTANT_Utf8, UTF8_PARSER],
	[CONSTANT_MethodHandle, METHOD_HANDLE_PARSER],
	[CONSTANT_MethodType, METHOD_TYPE_PARSER],
	[CONSTANT_InvokeDynamic, INVOKE_DYNAMIC_PARSER],
])

export const constantPoolParser: Parser<ConstantPool> =
	parseAndThen(parseShort, numConstants =>
		data => {
			let length = 0
			const pool = new ConstantPool(numConstants)
			for (let constantIndex = 1; constantIndex < numConstants;) {
				const constantTag = data.getUint8(length++)
				const constantParser = CONSTANT_PARSERS.get(constantTag)
				if (!constantParser) throw new Error(`Unknown constant pool type: ${constantTag}`)
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