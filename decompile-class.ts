import {AccessFlags} from './access-flags-parser'
import {blockToSections, IndentedLines, Section, sectionsToString} from './ast'
import {Code} from './bytecode-parser'
import {ClassFile} from './class-file-parser'
import {cleanup, convertClassString, getUsedVariables, resolvePackageClasses} from './cleanup-ast'
import {CodeAttribute} from './code-parser'
import {parseMethodAST} from './control-flow'
import {doubleWidthType, getArgTypes, getType} from './descriptor'
import {ExceptionTable} from './exception-table-parser'
import {LoadConstant, Stack} from './instructions'
import {processAttribute} from './process-attribute'
import {varName, getLocalTypes} from './variable-types'
import {inspect} from 'util'

const CODE = 'Code'
const CONSTANT_VALUE = 'ConstantValue'
const ACCESS_FLAGS_KEYS: (keyof AccessFlags)[] = [
	'public', 'private', 'protected',
	'static',
	'final',
	'transient',
	'native', 'abstract'
]

const accessFlagsToString = (flags: AccessFlags, isInterface = false) =>
	ACCESS_FLAGS_KEYS
		.filter(key => !(isInterface && key === 'abstract'))
		.map(key => flags[key] ? key + ' ' : '').join('')

function classToSections(clazz: ClassFile): Section[] {
	const {accessFlags, constantPool, fields, methods, thisClass, superClass} = clazz
	const isInterface = accessFlags.interface
	const className = thisClass.getValue(constantPool).name
	const packages = new Set<string>() //will have at most 1 member
	const shortClassName = convertClassString(className, packages)
	const imports = new Set<string>()
	const fieldsSections: Section[] = []
	for (const field of fields) {
		const {accessFlags, attributes, descriptor, name} = field
		const descriptorString = descriptor.getValue(constantPool),
		      nameString = name.getValue(constantPool)
		const typeString = convertClassString(getType(descriptorString), imports)
		let initializer: string | undefined
		for (const attribute of attributes) {
			const parsedAttribute = processAttribute({
				attribute,
				className,
				constantPool,
				method: field
			})
			if (parsedAttribute.type === CONSTANT_VALUE) {
				const stack: Stack = []
				new LoadConstant(parsedAttribute.value).execute(stack)
				initializer = stack[0].toString(true)
				break
			}
		}
		fieldsSections.push(
			accessFlagsToString(accessFlags) +
			`${typeString} ${nameString}${initializer ? ' = ' + initializer : ''};`
		)
	}
	const methodsSections: Section[] = []
	for (const method of methods) {
		const {accessFlags, attributes, name, descriptor} = method
		let instructions: Code | undefined,
		    exceptionTable: ExceptionTable | undefined
		for (const attribute of attributes) {
			const parsedAttribute = processAttribute({
				attribute,
				className,
				constantPool,
				method
			})
			if (parsedAttribute.type === CODE) {
				({instructions, exceptionTable} = parsedAttribute.value as CodeAttribute)
				console.log(name.getValue(constantPool))
				if (instructions.size < 100) console.log(inspect(instructions, false, Infinity, true))
				for (const {startPC, endPC, handlerPC, catchType} of exceptionTable) {
					console.log(`${startPC} - ${endPC} -> ${handlerPC}`, catchType)
				}
				console.log()
				break
			}
		}
		const descriptorString = descriptor.getValue(constantPool)
		const argTypes = new Map<number, string>()
		const params: string[] = []
		let paramLocalIndex = accessFlags.static ? 0 : 1
		for (const type of getArgTypes(descriptorString)) {
			argTypes.set(paramLocalIndex, type)
			params.push(`${convertClassString(type, imports)} ${varName(paramLocalIndex)}`)
			paramLocalIndex += doubleWidthType(type) ? 2 : 1
		}
		const returnType = getType(descriptorString)
		const declarations: string[] = []
		let methodSections: Section[]
		if (instructions) {
			const localTypes = getLocalTypes(instructions, paramLocalIndex)
			const block = cleanup(
				parseMethodAST(instructions, exceptionTable!),
				argTypes, localTypes, returnType
			)
			const importResolvedBlock = resolvePackageClasses(block, imports)
			methodSections = blockToSections(importResolvedBlock)
			const usedVariables = getUsedVariables(block)
			for (const [local, type] of localTypes) {
				if (usedVariables.has(local)) declarations.push(`${type} ${varName(local)};`)
			}
		}
		else methodSections = ['// Could not parse method']
		const nameString = name.getValue(constantPool)
		const signature = nameString === '<clinit>'
			? 'static'
			: accessFlagsToString(accessFlags, isInterface) +
				(nameString === '<init>'
					? shortClassName
					: convertClassString(returnType, imports) + ' ' +
						nameString
				) +
				`(${params.join(', ')})`
		if (accessFlags.abstract || accessFlags.native) methodsSections.push(signature + ';')
		else {
			methodsSections.push(
				signature + ' {',
				new IndentedLines(declarations),
				new IndentedLines(methodSections),
				'}'
			)
		}
	}
	const classType = isInterface
		? 'interface'
		: accessFlags.enum ? 'enum' : 'class'
	const superClassName = convertClassString(
		superClass.getValue(constantPool).name,
		imports
	)
	const [packageName] = packages as Set<string | undefined>
	const sections: Section[] = packageName
		? [`package ${packageName.slice(0, -(shortClassName.length + 1))};`, '']
		: []
	if (imports.size) {
		sections.push(
			...[...imports].sort()
				.map(importClass => `import ${importClass};`),
			''
		)
	}
	sections.push(
		accessFlagsToString(accessFlags, isInterface) +
			`${classType} ${shortClassName} extends ${superClassName} {`
	)
	if (fieldsSections.length) sections.push(new IndentedLines(fieldsSections), '')
	sections.push(new IndentedLines(methodsSections), '}')
	return sections
}

export const classToString = (classFile: ClassFile) =>
	sectionsToString(classToSections(classFile))