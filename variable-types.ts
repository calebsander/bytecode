import {Code} from './bytecode-parser';

const A = 'A'.charCodeAt(0)
function base26(n: number) {
	let str = ''
	do {
		str += String.fromCharCode(A + n % 26)
		n = (n / 26) | 0
	} while (n)
	return str
}
export const varName = (n: number) => `var${base26(n)}`

export type Primitive
	= 'boolean'
	| 'char'
	| 'float'
	| 'double'
	| 'byte'
	| 'short'
	| 'int'
	| 'long'
export type LocalType = Primitive | 'Object'
export function getLocalTypes(instructions: Code, localStartIndex: number): Map<number, string> {
	const loadStoreTypes = new Map<number, LocalType>()
	for (const {instruction} of instructions.values()) instruction.setLocalTypes(loadStoreTypes)
	const namedTypes = new Map<number, string>()
	for (const [n, type] of loadStoreTypes) {
		if (n >= localStartIndex) namedTypes.set(n, type)
	}
	/*
		TODO: get actual class of Object types - is this even possible?
		would require knowing external method signatures
		and recognizing interface types (e.g. something that gets assigned ArrayList and LinkedList is likely a List).
		Could at least cast when objects are used in methods.
	*/
	// TODO: differentiate bool/char/short/int, which are all stored as int locals
	return namedTypes
}