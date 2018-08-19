export interface ParseResult<E> {
	result: E
	length: number
}
export type Parser<E> = (data: DataView) => ParseResult<E>

export const slice = (data: DataView, offset: number): DataView =>
	new DataView(data.buffer, data.byteOffset + offset)

export const parseAndThen: <A, B>(parser: Parser<A>, f: (a: A) => Parser<B>) => Parser<B> =
	(parser, f) =>
		data => {
			const {result, length} = parser(data)
			const result2 = f(result)(slice(data, length))
			return {result: result2.result, length: length + result2.length}
		}
export const parseReturn: <A>(a: A) => Parser<A> = a => _ => ({result: a, length: 0})
export const parseByte: Parser<number> = data => {
	return {
		result: data.getUint8(0),
		length: 1
	}
}
export const parseShort: Parser<number> = data => {
	return {
		result: data.getUint16(0),
		length: 2
	}
}
export const parseInt: Parser<number> = data => {
	return {
		result: data.getUint32(0),
		length: 4
	}
}
export const parseSignedInt: Parser<number> = data => {
	return {
		result: data.getInt32(0),
		length: 4
	}
}
export const parseByteArray = (lengthParser: Parser<number>): Parser<ArrayBuffer> =>
	parseAndThen(lengthParser, bytes =>
		data => ({
			result: data.buffer.slice(data.byteOffset, data.byteOffset + bytes),
			length: bytes
		})
	)
export const parseTimes = <E>(parser: Parser<E>, n: number): Parser<E[]> => data => {
	let length = 0
	const result: E[] = []
	while (n--) {
		const parseResult = parser(slice(data, length))
		length += parseResult.length
		result.push(parseResult.result)
	}
	return {result, length}
}
export const parseRepeated = <E>(parser: Parser<E>): Parser<E[]> =>
	parseAndThen(parseShort, count => parseTimes(parser, count))
export const parseStruct = <E>(parsers: [keyof E, Parser<any>][]): Parser<E> => data => {
	let length = 0
	const result = {} as E
	for (const [field, parser] of parsers) {
		const parseResult = parser(slice(data, length))
		length += parseResult.length
		result[field] = parseResult.result
	}
	return {result, length}
}