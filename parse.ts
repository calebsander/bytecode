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
export const parseByteArray: (lengthParser: Parser<number>) => Parser<ArrayBuffer> =
	lengthParser =>
		parseAndThen(lengthParser, bytes =>
			data => ({
				result: data.buffer.slice(data.byteOffset, data.byteOffset + bytes),
				length: bytes
			})
		)
const parseTimes: <E>(parser: Parser<E>, n: number) => Parser<E[]> =
	(parser, n) =>
		n
			? parseAndThen(parser, head =>
					parseAndThen(parseTimes(parser, n - 1), tail =>
						parseReturn([head, ...tail])
					)
				)
			: parseReturn([])
export const parseRepeated: <E>(parser: Parser<E>) => Parser<E[]> =
	parser =>
		parseAndThen(parseShort, count => parseTimes(parser, count))
const parseStructFields: <E>(parsers: [keyof E, Parser<any>][], i: number) => Parser<E> =
	<E>(parsers: [keyof E, Parser<any>][], i: number) => {
		if (i === parsers.length) return parseReturn({} as E)
		const [field, parser] = parsers[i]
		return parseAndThen(parser, value =>
			parseAndThen(parseStructFields(parsers, i + 1), rest => {
				rest[field] = value
				return parseReturn(rest)
			})
		)
	}
export const parseStruct: <E>(parsers: [keyof E, Parser<any>][]) => Parser<E> =
	parsers => parseStructFields(parsers, 0)