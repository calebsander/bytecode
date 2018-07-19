function parseDescriptor(descriptor: string, args: boolean): string[] {
	const types: string[] = []
	let arrDepth = 0
	let start: number
	if (args) {
		if (descriptor[0] !== '(') throw new Error('Expected (')
		start = 1
	}
	else start = descriptor.indexOf(')') + 1 //also works if there are no parens
	charLoop: for (let i = start; i < descriptor.length; i++) {
		let type: string | undefined
		switch (descriptor[i]) {
			case 'L':
				i++
				const semicolon = descriptor.indexOf(';', i)
				if (semicolon === -1) throw new Error('Missing semicolon in descriptor')
				type = descriptor.substring(i, semicolon)
				i = semicolon
				break
			case 'B':
				type = 'byte'
				break
			case 'C':
				type = 'char'
				break
			case 'D':
				type = 'double'
				break
			case 'F':
				type = 'float'
				break
			case 'I':
				type = 'int'
				break
			case 'J':
				type = 'long'
				break
			case 'S':
				type = 'short'
				break
			case 'V':
				if (arrDepth) throw new Error('Cannot have array of type void')
				if (args) throw new Error('Cannot have void arg')
				type = 'void'
				break
			case 'Z':
				type = 'boolean'
				break
			case '[':
				arrDepth++
				break
			case ')':
				if (!args) throw new Error('Unexpected )')
				break charLoop
			default:
				throw new Error('Unexpected character in descriptor: ' + descriptor[i])
		}
		if (type) {
			types.push(type + '[]'.repeat(arrDepth))
			arrDepth = 0
		}
	}
	return types
}

export const getArgTypes = (descriptor: string) =>
	parseDescriptor(descriptor, true)
export const getType = (descriptor: string) => {
	const types = parseDescriptor(descriptor, false)
	if (types.length !== 1) throw new Error('Expected 1 type')
	return types[0]
}
export const doubleWidthType = (type: string) =>
	type === 'long' || type === 'double'