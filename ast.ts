import {varName} from './variable-types'

export type ExpressionHandler = (exp: Expression) => void
export type Section = IndentedLines | string
export class IndentedLines {
	static readonly INDENT = ' '.repeat(2)
	constructor(readonly sections: Section[]) {}
	toLines(indentLevel = 1): string[] {
		const lines: string[] = []
		for (const section of this.sections) {
			if (section instanceof IndentedLines) {
				lines.push(...section.toLines(indentLevel + 1))
			}
			else lines.push(IndentedLines.INDENT.repeat(indentLevel) + section)
		}
		return lines
	}
}

export interface Expression {
	readonly doubleWidth: boolean
	maybeBoolean(notBooleanVars: Set<number>): boolean
	walk(handler: ExpressionHandler): void
	replace(replacements: Map<Expression, Expression>): Expression
	toString(omitParens?: boolean): string
}
interface PrimitiveExpression extends Expression {}
abstract class PrimitiveExpression {
	walk(handler: ExpressionHandler) { handler(this) }
	replace() { return this }
}
export class IntegerLiteral extends PrimitiveExpression {
	constructor(
		readonly i: number | bigint,
		readonly doubleWidth = false
	) { super() }
	maybeBoolean() { return this.i === 0 || this.i === 1 }
	toString() {
		return `${this.i}${this.doubleWidth ? 'L' : ''}`
	}
}
export class FloatLiteral extends PrimitiveExpression {
	constructor(
		readonly f: number,
		readonly doubleWidth = false
	) { super() }
	maybeBoolean() { return false }
	toString() {
		return `${this.f}${this.doubleWidth ? '' : 'F'}`
	}
}
export class BooleanLiteral extends PrimitiveExpression {
	constructor(readonly b: boolean) { super() }
	get doubleWidth() { return false }
	maybeBoolean() { return true }
	toString() { return `${this.b}` }
}
export class StringLiteral extends PrimitiveExpression {
	constructor(readonly str: string) { super() }
	get doubleWidth() { return false }
	maybeBoolean() { return false }
	toString() {
		return `"${this.str.replace(/\\/g, '\\\\').replace(/"/g, '\\"')}"`
	}
}
export class ClassLiteral extends PrimitiveExpression {
	constructor(readonly clazz: NameReference) { super() }
	get doubleWidth() { return false }
	maybeBoolean() { return false }
	toString() { return this.clazz.name + '.class' }
}
export class NullLiteral extends PrimitiveExpression {
	get doubleWidth() { return false }
	maybeBoolean() { return false }
	toString() { return 'null' }
}
export class ThisLiteral extends PrimitiveExpression {
	constructor(readonly isSuper = false) { super() }
	get doubleWidth() { return false }
	maybeBoolean() { return false }
	toString() { return this.isSuper ? 'super' : 'this' }
}
export class Variable extends PrimitiveExpression {
	constructor(
		readonly n: number,
		readonly doubleWidth = false
	) { super() }
	maybeBoolean(notBooleanVars: Set<number>) {
		return !notBooleanVars.has(this.n)
	}
	toString() { return varName(this.n) }
}
export type IncDec = '++' | '--'
export type UnaryOp
	= '-'
	| '!'
	| '~'
	| {op: IncDec, post: boolean}
export class UnaryOperation implements Expression {
	constructor(
		readonly op: UnaryOp,
		readonly arg: Expression
	) {}
	get doubleWidth() { return this.arg.doubleWidth }
	maybeBoolean() { return this.op === '!' }
	walk(handler: ExpressionHandler) {
		handler(this)
		this.arg.walk(handler)
	}
	replace(replacements: Map<Expression, Expression>) {
		return new UnaryOperation(
			this.op,
			(replacements.get(this.arg) || this.arg).replace(replacements)
		)
	}
	toString() {
		const op: string = (this.op as {op: IncDec}).op || this.op
		const post = (this.op as {post: boolean}).post || false
		const argString = this.arg.toString()
		return post
			? argString + op
			: op + argString
	}
}
export const ASSIGNMENT_BINARY_OPS = {
	'+': true,
	'-': true,
	'*': true,
	'/': true,
	'%': true,
	'<<': true,
	'>>': true,
	'>>>': true,
	'&': true,
	'|': true,
	'^': true
}
export type AssignmentBinaryOp = keyof typeof ASSIGNMENT_BINARY_OPS
export type BinaryOp
	= AssignmentBinaryOp
	| '<' | '<=' | '>' | '>='
	| '==' | '!='
	| '&&' | '||'
	| 'instanceof'
export class BinaryOperation implements Expression {
	constructor(
		readonly op: BinaryOp,
		readonly arg1: Expression,
		readonly arg2: Expression
	) {}
	get doubleWidth() {
		if (this.arg1.doubleWidth !== this.arg2.doubleWidth) {
			throw new Error('Mismatched widths')
		}
		return this.arg1.doubleWidth
	}
	maybeBoolean(notBooleanVars: Set<number>) {
		return (
			(this.op === '&' || this.op === '|' || this.op === '^') &&
			this.arg1.maybeBoolean(notBooleanVars) && this.arg2.maybeBoolean(notBooleanVars)
		) || !(this.op in ASSIGNMENT_BINARY_OPS)
	}
	walk(handler: ExpressionHandler) {
		handler(this)
		this.arg1.walk(handler)
		this.arg2.walk(handler)
	}
	replace(replacements: Map<Expression, Expression>) {
		return new BinaryOperation(
			this.op,
			(replacements.get(this.arg1) || this.arg1).replace(replacements),
			(replacements.get(this.arg2) || this.arg2).replace(replacements)
		)
	}
	toString(omitParens?: boolean) {
		const insideParens = `${this.arg1.toString()} ${this.op} ${this.arg2.toString()}`
		return omitParens ? insideParens : `(${insideParens})`
	}
}
export class Ternary implements Expression {
	constructor(
		readonly cond: Expression,
		readonly ifTrue: Expression,
		readonly ifFalse: Expression
	) {}
	get doubleWidth() {
		if (this.ifTrue.doubleWidth !== this.ifFalse.doubleWidth) {
			throw new Error('Mismatched widths')
		}
		return this.ifTrue.doubleWidth
	}
	maybeBoolean(notBooleanVars: Set<number>) {
		return this.ifTrue.maybeBoolean(notBooleanVars) && this.ifFalse.maybeBoolean(notBooleanVars)
	}
	walk(handler: ExpressionHandler) {
		handler(this)
		this.cond.walk(handler)
		this.ifTrue.walk(handler)
		this.ifFalse.walk(handler)
	}
	replace(replacements: Map<Expression, Expression>) {
		return new Ternary(
			(replacements.get(this.cond) || this.cond).replace(replacements),
			(replacements.get(this.ifTrue) || this.ifTrue).replace(replacements),
			(replacements.get(this.ifFalse) || this.ifFalse).replace(replacements)
		)
	}
	toString(omitParens?: boolean) {
		const insideParens =
			`${this.cond.toString()} ? ${this.ifTrue.toString()} : ${this.ifFalse.toString()}`
		return omitParens ? insideParens : `(${insideParens})`
	}
}
export class Assignment implements Expression {
	constructor(
		readonly lhs: Variable | ArrayAccess | FieldAccess,
		readonly rhs: Expression,
		readonly op?: AssignmentBinaryOp
	) {}
	get doubleWidth() { return this.rhs.doubleWidth }
	maybeBoolean(notBooleanVars: Set<number>) {
		return (!this.op || this.op === '&' || this.op === '|' || this.op === '^') &&
			(this.lhs as Expression).maybeBoolean(notBooleanVars) &&
			this.rhs.maybeBoolean(notBooleanVars)
	}
	walk(handler: ExpressionHandler) {
		handler(this)
		this.lhs.walk(handler)
		this.rhs.walk(handler)
	}
	replace(replacements: Map<Expression, Expression>) {
		return new Assignment(
			(replacements.get(this.lhs) || this.lhs).replace(replacements) as Variable | ArrayAccess | FieldAccess,
			(replacements.get(this.rhs) || this.rhs).replace(replacements),
			this.op
		)
	}
	toString(omitParens?: boolean) {
		const insideParens = `${this.lhs.toString()} ${this.op || ''}= ${this.rhs.toString(true)}` //are parens needed on RHS?
		return omitParens ? insideParens : `(${insideParens})`
	}
}
export interface NameReference {
	readonly name: string
}
export class ClassReference extends PrimitiveExpression {
	constructor(readonly clazz: NameReference) { super() }
	get doubleWidth() { return false }
	maybeBoolean() { return false }
	toString() { return this.clazz.name }
}
export class FunctionCall implements Expression {
	constructor(
		readonly obj: Expression,
		readonly func: NameReference | null, //null if calling a constructor
		readonly args: Expression[],
		readonly argTypes: string[],
		readonly doubleWidth: boolean,
		readonly isBoolean: boolean
	) {}
	maybeBoolean() { return this.isBoolean }
	walk(handler: ExpressionHandler) {
		handler(this)
		this.obj.walk(handler)
		for (const arg of this.args) arg.walk(handler)
	}
	replace(replacements: Map<Expression, Expression>) {
		return new FunctionCall(
			(replacements.get(this.obj) || this.obj).replace(replacements),
			this.func,
			this.args.map(arg =>
				(replacements.get(arg) || arg).replace(replacements)
			),
			this.argTypes,
			this.doubleWidth,
			this.isBoolean
		)
	}
	toString() {
		const {obj, func, args} = this
		return obj.toString() +
		       (func ? '.' + func.name : '') +
		       `(${args.map(arg => arg.toString(true)).join(', ')})`
	}
}
export class FieldAccess implements Expression {
	constructor(
		readonly obj: Expression,
		readonly field: NameReference,
		readonly doubleWidth: boolean,
		readonly isBoolean: boolean
	) {}
	maybeBoolean() { return this.isBoolean }
	walk(handler: ExpressionHandler) {
		handler(this)
		this.obj.walk(handler)
	}
	replace(replacements: Map<Expression, Expression>) {
		return new FieldAccess(
			(replacements.get(this.obj) || this.obj).replace(replacements),
			this.field,
			this.doubleWidth,
			this.isBoolean
		)
	}
	toString() {
		return `${this.obj.toString()}.${this.field.name}`
	}
}
export class ArrayAccess implements Expression {
	constructor(
		readonly arr: Expression,
		readonly index: Expression,
		readonly doubleWidth: boolean
	) {}
	maybeBoolean() { return false }
	walk(handler: ExpressionHandler) {
		handler(this)
		this.arr.walk(handler)
		this.index.walk(handler)
	}
	replace(replacements: Map<Expression, Expression>) {
		return new ArrayAccess(
			(replacements.get(this.arr) || this.arr).replace(replacements),
			(replacements.get(this.index) || this.index).replace(replacements),
			this.doubleWidth
		)
	}
	toString() {
		return `${this.arr.toString()}[${this.index.toString(true)}]`
	}
}
export class NewObject implements Expression {
	constructor(
		readonly clazz: NameReference,
		public args?: Expression[]
	) {}
	get doubleWidth() { return false }
	maybeBoolean() { return false }
	walk(handler: ExpressionHandler) {
		handler(this)
		for (const arg of this.args || []) arg.walk(handler)
	}
	replace(replacements: Map<Expression, Expression>) {
		return new NewObject(
			this.clazz,
			this.args && this.args.map(arg =>
				(replacements.get(arg) || arg).replace(replacements)
			)
		)
	}
	toString() {
		if (!this.args) throw new Error('No arguments given to constructor')
		return `new ${this.clazz.name}(${this.args
			.map(arg => arg.toString(true))
			.join(', ')
		})`
	}
}
export class NewArray implements Expression {
	constructor(
		readonly type: NameReference,
		readonly dimensions: Expression[],
		readonly primitive: boolean,
		public elements?: Expression[]
	) {}
	get doubleWidth() { return false }
	maybeBoolean() { return false }
	walk(handler: ExpressionHandler) {
		handler(this)
		for (const dimension of this.dimensions) dimension.walk(handler)
		for (const element of this.elements || []) element.walk(handler)
	}
	replace(replacements: Map<Expression, Expression>) {
		return new NewArray(
			this.type,
			this.dimensions.map(dimension =>
				(replacements.get(dimension) || dimension).replace(replacements)
			),
			this.primitive,
			this.elements && this.elements.map(element =>
				(replacements.get(element) || element).replace(replacements)
			)
		)
	}
	toString() {
		return `new ${this.type.name}${this.dimensions
			.map(dimension => `[${this.elements ? '' : dimension.toString(true)}]`)
			.join('')
		}${this.elements
			? `{${this.elements
				.map(element => element.toString())
				.join(', ')
			}}`
			: ''
		}`
	}
}
export class Cast implements Expression {
	constructor(
		readonly type: NameReference,
		readonly exp: Expression,
		readonly primitive: boolean
	) {}
	get doubleWidth() {
		const {name} = this.type
		return name === 'long' || name === 'double'
	}
	maybeBoolean() { return false }
	walk(handler: ExpressionHandler) {
		handler(this)
		this.exp.walk(handler)
	}
	replace(replacements: Map<Expression, Expression>) {
		return new Cast(
			this.type,
			(replacements.get(this.exp) || this.exp).replace(replacements),
			this.primitive
		)
	}
	toString(omitParens?: boolean) {
		const insideParens = `(${this.type.name})${this.exp.toString()}`
		return omitParens ? insideParens : `(${insideParens})`
	}
}

export type StatementHandler = (statement: Statement) => void
export type BlockHandler = (block: Block) => void
export interface Replacements {
	expressions: Map<Expression, Expression>
	statements: Map<Statement, Block>
}

export interface Statement {
	walkExpressions(handler: ExpressionHandler): void
	walkBlocks(handler: BlockHandler): void
	replace(replacements: Replacements): Statement
	toSections(enclosingLoop?: LoopReference): Section[]
}
interface PrimitiveStatement extends Statement {}
abstract class PrimitiveStatement {
	walkBlocks() {}
}
export class ExpressionStatement extends PrimitiveStatement {
	constructor(readonly exp: Expression) { super() }
	walkExpressions(handler: ExpressionHandler) { this.exp.walk(handler) }
	replace({expressions}: Replacements) {
		return new ExpressionStatement(
			(expressions.get(this.exp) || this.exp).replace(expressions)
		)
	}
	toSections() {
		return [this.exp.toString(true) + ';']
	}
}
export interface LoopReference {
	readonly label: string
}
export class BreakStatement extends PrimitiveStatement {
	constructor(readonly loop: LoopReference) { super() }
	walkExpressions() {}
	replace() { return this }
	toSections(enclosingLoop?: LoopReference) {
		return [`break${enclosingLoop === this.loop ? '' : ' ' + this.loop.label};`]
	}
}
export class ContinueStatement extends PrimitiveStatement {
	constructor(readonly loop: LoopReference) { super() }
	walkExpressions() {}
	replace() { return this }
	toSections(enclosingLoop?: LoopReference) {
		return [`continue${enclosingLoop === this.loop ? '' : ' ' + this.loop.label};`]
	}
}
export class ReturnStatement extends PrimitiveStatement {
	constructor(readonly exp?: Expression) { super() }
	walkExpressions(handler: ExpressionHandler) {
		if (this.exp) this.exp.walk(handler)
	}
	replace({expressions}: Replacements) {
		return new ReturnStatement(
			this.exp && (expressions.get(this.exp) || this.exp).replace(expressions)
		)
	}
	toSections() {
		return [`return${this.exp ? ' ' + this.exp.toString(true) : ''};`]
	}
}
export class ThrowStatement extends PrimitiveStatement {
	constructor(readonly err: Expression) { super() }
	walkExpressions(handler: ExpressionHandler) { this.err.walk(handler) }
	replace({expressions}: Replacements) {
		return new ThrowStatement(
			(expressions.get(this.err) || this.err).replace(expressions)
		)
	}
	toSections() {
		return [`throw ${this.err.toString(true)};`]
	}
}
export class IfStatement implements Statement {
	constructor(
		readonly cond: Expression,
		readonly ifBlock: Block,
		readonly elseBlock: Block = []
	) {}
	walkExpressions(handler: ExpressionHandler) {
		this.cond.walk(handler)
		for (const statement of this.ifBlock) statement.walkExpressions(handler)
		for (const statement of this.elseBlock) statement.walkExpressions(handler)
	}
	walkBlocks(handler: BlockHandler) {
		handler(this.ifBlock)
		handler(this.elseBlock)
		for (const statement of this.ifBlock) statement.walkBlocks(handler)
		for (const statement of this.elseBlock) statement.walkBlocks(handler)
	}
	replace(replacements: Replacements) {
		const {expressions} = replacements
		return new IfStatement(
			(expressions.get(this.cond) || this.cond).replace(expressions),
			replaceBlock(this.ifBlock, replacements),
			replaceBlock(this.elseBlock, replacements)
		)
	}
	toSections(enclosingLoop: LoopReference) {
		const ifCond = `if (${this.cond.toString(true)}) `
		const ifSections = blockToSections(this.ifBlock, enclosingLoop)
		const sections: Section[] = ifSections.length === 1
			? [ifCond + ifSections[0]]
			: [
					ifCond + '{',
					new IndentedLines(ifSections),
					'}'
				]
		const {elseBlock} = this
		if (!elseBlock.length) return sections

		if (elseBlock.length === 1 && elseBlock[0] instanceof IfStatement) {
			const elseSections = elseBlock[0].toSections(enclosingLoop)
			elseSections[0] = `else ${elseSections[0] as string}`
			sections.push(...elseSections)
		}
		else {
			const elseSections = blockToSections(elseBlock, enclosingLoop)
			if (elseSections.length === 1 && typeof elseSections[0] === 'string') {
				sections.push('else ' + elseSections[0])
			}
			else {
				sections.push(
					'else {',
					new IndentedLines(elseSections),
					'}'
				)
			}
		}
		return sections
	}
}
interface LabeledStatement extends Statement {}
abstract class LabeledStatement {
	constructor(readonly label: LoopReference) {}
}
export class WhileStatement extends LabeledStatement {
	constructor(
		readonly cond: Expression,
		readonly block: Block,
		readonly doWhile: boolean,
		label: LoopReference
	) { super(label) }
	walkExpressions(handler: ExpressionHandler) {
		this.cond.walk(handler)
		for (const statement of this.block) statement.walkExpressions(handler)
	}
	walkBlocks(handler: BlockHandler) {
		handler(this.block)
		for (const statement of this.block) statement.walkBlocks(handler)
	}
	replace(replacements: Replacements) {
		const {expressions} = replacements
		return new WhileStatement(
			(expressions.get(this.cond) || this.cond).replace(expressions),
			replaceBlock(this.block, replacements),
			this.doWhile,
			this.label
		)
	}
	toSections() {
		const blockSections = blockToSections(this.block, this.label)
		const labelString: string = isLabelNeeded(this) ? this.label.label + ': ' : ''
		const whileCond = `while (${this.cond.toString(true)})`
		if (this.doWhile) {
			return [
				labelString + 'do {',
				new IndentedLines(blockSections),
				`} ${whileCond};`
			]
		}
		else {
			return blockSections.length === 1
				? [labelString + whileCond + ' ' + blockSections[0]]
				: [
						labelString + whileCond + ' {',
						new IndentedLines(blockSections),
						'}'
					]
		}
	}
}
export class ForInStatement extends LabeledStatement {
	constructor(
		readonly type: NameReference,
		readonly variable: Variable,
		readonly iterable: Expression,
		readonly block: Block,
		label: LoopReference
	) { super(label) }
	walkExpressions(handler: ExpressionHandler) {
		this.iterable.walk(handler)
		for (const statement of this.block) statement.walkExpressions(handler)
	}
	walkBlocks(handler: BlockHandler) {
		handler(this.block)
		for (const statement of this.block) statement.walkBlocks(handler)
	}
	replace(replacements: Replacements) {
		const {expressions} = replacements
		return new ForInStatement(
			this.type,
			this.variable,
			(expressions.get(this.iterable) || this.iterable).replace(expressions),
			replaceBlock(this.block, replacements),
			this.label
		)
	}
	toSections() {
		const blockSections = blockToSections(this.block, this.label)
		const forCond: string = (isLabelNeeded(this) ? this.label.label + ': ' : '') +
			`for (${this.type.name} ${this.variable.toString()} : ${this.iterable.toString(true)}) `
		return blockSections.length === 1
			? [forCond + blockSections[0]]
			: [
				forCond + '{',
				new IndentedLines(blockSections),
				'}'
			]
	}
}
export interface Case {
	exp: Expression | null //null for default
	block: Block
}
export class SwitchStatement extends LabeledStatement {
	constructor(
		readonly val: Expression,
		readonly cases: Case[],
		label: LoopReference
	) { super(label) }
	walkExpressions(handler: ExpressionHandler) {
		this.val.walk(handler)
		for (const {exp, block} of this.cases) {
			if (exp) exp.walk(handler)
			for (const statement of block) statement.walkExpressions(handler)
		}
	}
	walkBlocks(handler: BlockHandler) {
		for (const {block} of this.cases) {
			handler(block)
			for (const statement of block) statement.walkBlocks(handler)
		}
	}
	replace(replacements: Replacements) {
		const {expressions} = replacements
		return new SwitchStatement(
			(expressions.get(this.val) || this.val).replace(expressions),
			this.cases.map(({exp, block}) => ({
				exp: exp && exp.replace(expressions),
				block: replaceBlock(block, replacements)
			})),
			this.label
		)
	}
	toSections() {
		const innerSections: Section[] = []
		for (const {exp, block} of this.cases) {
			innerSections.push(
				(exp ? 'case ' + exp.toString(true) : 'default') + ':',
				new IndentedLines(blockToSections(block, this.label))
			)
		}
		const labelString: string = isLabelNeeded(this) ? this.label.label + ': ' : ''
		return [
			`${labelString}switch (${this.val.toString(true)}) {`,
			new IndentedLines(innerSections),
			'}'
		]
	}
}
export class SynchronizedStatement implements Statement {
	constructor(
		readonly obj: Expression,
		readonly block: Block
	) {}
	walkExpressions(handler: ExpressionHandler) {
		this.obj.walk(handler)
		for (const statement of this.block) statement.walkExpressions(handler)
	}
	walkBlocks(handler: BlockHandler) {
		handler(this.block)
		for (const statement of this.block) statement.walkBlocks(handler)
	}
	replace(replacements: Replacements) {
		const {expressions} = replacements
		return new SynchronizedStatement(
			(expressions.get(this.obj) || this.obj).replace(expressions),
			replaceBlock(this.block, replacements)
		)
	}
	toSections(enclosingLoop?: LoopReference) {
		return [
			`synchronized (${this.obj.toString(true)}) {`,
			new IndentedLines(blockToSections(this.block, enclosingLoop)),
			'}'
		]
	}
}
export interface Catch {
	types: ClassReference[] //[] for finally
	variable: Variable
	block: Block
}
export class TryStatement implements Statement {
	constructor(
		readonly tryBlock: Block,
		readonly catches: Catch[]
	) {}
	walkExpressions(handler: ExpressionHandler) {
		for (const statement of this.tryBlock) statement.walkExpressions(handler)
		for (const {types, block} of this.catches) {
			for (const type of types) type.walk(handler)
			for (const statement of block) statement.walkExpressions(handler)
		}
	}
	walkBlocks(handler: BlockHandler) {
		handler(this.tryBlock)
		for (const statement of this.tryBlock) statement.walkBlocks(handler)
		for (const {block} of this.catches) {
			handler(block)
			for (const statement of block) statement.walkBlocks(handler)
		}
	}
	replace(replacements: Replacements) {
		const {expressions} = replacements
		return new TryStatement(
			replaceBlock(this.tryBlock, replacements),
			this.catches.map(({types, variable, block}) => ({
				types: types.map(type => expressions.get(type) as ClassReference || type),
				variable,
				block: replaceBlock(block, replacements)
			}))
		)
	}
	toSections(enclosingLoop?: LoopReference) {
		return [
			'try {',
			new IndentedLines(blockToSections(this.tryBlock, enclosingLoop)),
			'}',
			...flatten(this.catches.map(({types, variable, block}) => [
				(types.length
					? `catch (${types.map(type => type.toString()).join(' | ')} ${variable.toString()})`
					: 'finally'
				) + ' {',
				new IndentedLines(blockToSections(block, enclosingLoop)),
				'}'
			]))
		]
	}
}

export type Block = Statement[]
export const flatten = <T>(segments: T[][]): T[] =>
	([] as T[]).concat(...segments)
export function replaceBlock(block: Block, replacements: Replacements): Block {
	const {statements} = replacements
	return flatten(block.map(statement => statements.get(statement) || [statement]))
		.map(statement => statement.replace(replacements))
}
export const blockToSections = (block: Block, enclosingLoop?: LoopReference) =>
	flatten(block.map(statement => statement.toSections(enclosingLoop)))
export const sectionsToString = (sections: Section[]) =>
	flatten(sections.map(section =>
		section instanceof IndentedLines ? section.toLines() : [section]
	)).join('\n')
export const blockToString = (block: Block) =>
	sectionsToString(blockToSections(block))
function isLabelNeeded(loopOrSwitch: LabeledStatement) {
	const {label} = loopOrSwitch
	let labelNeeded = false
	loopOrSwitch.walkBlocks(block => { //look for break/continue referencing this loop inside a different loop
		for (const statement of block) {
			if (statement instanceof LabeledStatement) {
				statement.walkBlocks(block => {
					for (const statement of block) {
						if (statement instanceof BreakStatement || statement instanceof ContinueStatement) {
							if (statement.loop === label) labelNeeded = true
						}
					}
				})
			}
		}
	})
	return labelNeeded
}