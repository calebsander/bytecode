import {AccessFlags} from './access-flags-parser'
import {blockToSections, IndentedLines, Section, sectionsToString} from './ast'
import {Code} from './bytecode-parser'
import {ClassFile} from './class-file-parser'
import {cleanup, convertClassString, resolvePackageClasses} from './cleanup-ast'
import {CodeAttribute} from './code-parser'
import {parseMethodAST} from './control-flow'
import {doubleWidthType, getArgTypes, getType} from './descriptor'
import {processAttribute} from './process-attribute'
import {varName, getLocalTypes} from './variable-types'

const CODE = 'Code'
const ACCESS_FLAGS_KEYS: (keyof AccessFlags)[] = [
	'public', 'private', 'protected',
	'static',
	'final',
	'transient',
	'native', 'abstract'
]

const accessFlagsToString = (flags: AccessFlags) =>
	ACCESS_FLAGS_KEYS.map(key => flags[key] ? key + ' ' : '').join('')

const convertTypeString = (type: string, imports: Set<string>) =>
	type.includes('/')
		? convertClassString(type, imports)
		: type

function classToSections(clazz: ClassFile): Section[] {
	const {accessFlags, constantPool, methods, thisClass, superClass} = clazz
	const className = thisClass.getValue(constantPool).name
	const imports = new Set<string>()
	const methodsSections: Section[] = []
	for (const method of methods) {
		const {accessFlags, attributes, name, descriptor} = method
		let instructions: Code | undefined
		for (const attribute of attributes) {
			const parsedAttribute = processAttribute({
				attribute,
				className,
				constantPool,
				method
			})
			if (parsedAttribute.type === CODE) {
				({instructions} = parsedAttribute.value as CodeAttribute)
				console.log(name.getValue(constantPool))
				console.log(instructions)
				console.log()
				break
			}
		}
		const descriptorString = descriptor.getValue(constantPool)
		const params: string[] = []
		let paramLocalIndex = accessFlags.static ? 0 : 1
		for (const type of getArgTypes(descriptorString)) {
			params.push(`${convertTypeString(type, imports)} ${varName(paramLocalIndex)}`)
			paramLocalIndex += doubleWidthType(type) ? 2 : 1
		}
		const declarations: string[] = []
		let methodSections: Section[]
		if (instructions) {
			const block = parseMethodAST(instructions)
			const cleanedBlock = cleanup(block)
			const importResolvedBlock = resolvePackageClasses(cleanedBlock, imports)
			methodSections = blockToSections(importResolvedBlock)
			const localTypes = getLocalTypes(instructions, paramLocalIndex)
			for (const [local, type] of localTypes) declarations.push(`${type} ${local};`)
		}
		else methodSections = ['// Could not parse method']
		const nameString = name.getValue(constantPool)
		methodsSections.push(
			accessFlagsToString(accessFlags) +
				(nameString === '<init>'
					? className
					: convertTypeString(getType(descriptorString), imports) + ' ' +
						nameString
				) +
				`(${params.join(', ')}) {`,
			new IndentedLines(declarations),
			new IndentedLines(methodSections),
			'}'
		)
	}
	const classType = accessFlags.interface
		? 'interface'
		: accessFlags.enum ? 'enum' : 'class'
	const superClassName = convertClassString(
		superClass.getValue(constantPool).name,
		imports
	)
	return [
		...[...imports].sort().map(importClass => `import ${importClass};`),
		'',
		accessFlagsToString(accessFlags) +
			`${classType} ${className} extends ${superClassName} {`,
		new IndentedLines(methodsSections),
		'}'
	]
}

export const classToString = (classFile: ClassFile) =>
	sectionsToString(classToSections(classFile))