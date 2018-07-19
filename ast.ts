import {varName} from './variable-types'

export type Numeric = number | BigInt
export type ExpressionHandler = (exp: Expression) => void
export type Section = IndentedLines | string
export class IndentedLines {
	static readonly INDENT: string = ' '.repeat(2)
	constructor(public readonly sections: Section[]) {}
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

export abstract class Expression {
	abstract readonly doubleWidth: boolean
	abstract walk(handler: ExpressionHandler): void
	abstract replace(replacements: Map<Expression, Expression>): Expression
	abstract toString(omitParens?: boolean): string
}
abstract class PrimitiveExpression extends Expression {
	walk(handler: ExpressionHandler) { handler(this) }
	replace() { return this }
}
export class IntegerLiteral extends PrimitiveExpression {
	constructor(
		public readonly i: Numeric,
		public readonly doubleWidth = false
	) { super() }
	toString() {
		return `${this.i}${this.doubleWidth ? 'L' : ''}`
	}
}
export class FloatLiteral extends PrimitiveExpression {
	constructor(
		public readonly f: number,
		public readonly doubleWidth = false
	) { super() }
	toString() {
		return `${this.f}${this.doubleWidth ? '' : 'F'}`
	}
}
export class BooleanLiteral extends PrimitiveExpression {
	constructor(public readonly b: boolean) { super() }
	get doubleWidth() { return false }
	toString() { return `${this.b}` }
}
export class StringLiteral extends PrimitiveExpression {
	constructor(public readonly str: string) { super() }
	get doubleWidth() { return false }
	toString() {
		return `"${this.str.replace(/\\/g, '\\\\').replace(/"/g, '\\"')}"`
	}
}
export class NullLiteral extends PrimitiveExpression {
	get doubleWidth() { return false }
	toString() { return 'null' }
}
export class ThisLiteral extends PrimitiveExpression {
	constructor(public readonly isSuper = false) { super() }
	get doubleWidth() { return false }
	toString() { return this.isSuper ? 'super' : 'this' }
}
export class Variable extends PrimitiveExpression {
	constructor(
		public readonly n: number,
		public readonly doubleWidth = false
	) { super() }
	toString() { return varName(this.n) }
}
export type IncDec = '++' | '--'
export type UnaryOp
	= '-'
	| '!'
	| '~'
	| {op: IncDec, post: boolean}
export class UnaryOperation extends Expression {
	constructor(
		public readonly op: UnaryOp,
		public readonly arg: Expression
	) { super() }
	get doubleWidth() { return this.arg.doubleWidth }
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
export type BinaryOp
	= '*' | '/' | '%'
	| '+' | '-'
	| '<<' | '>>' | '>>>'
	| '<' | '<=' | '>' | '>='
	| '==' | '!='
	| '&' | '^' | '|'
	| '&&' | '||'
export class BinaryOperation extends Expression {
	constructor(
		public readonly op: BinaryOp,
		public readonly arg1: Expression,
		public readonly arg2: Expression
	) { super() }
	get doubleWidth() {
		if (this.arg1.doubleWidth !== this.arg2.doubleWidth) {
			throw new Error('Mismatched widths')
		}
		return this.arg1.doubleWidth
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
export class Ternary extends Expression {
	constructor(
		public readonly cond: Expression,
		public readonly ifTrue: Expression,
		public readonly ifFalse: Expression
	) { super() }
	get doubleWidth() {
		if (this.ifTrue.doubleWidth !== this.ifFalse.doubleWidth) {
			throw new Error('Mismatched widths')
		}
		return this.ifTrue.doubleWidth
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
export class Assignment extends Expression {
	constructor(
		public readonly lhs: Variable | ArrayAccess,
		public readonly rhs: Expression
	) { super() }
	get doubleWidth() { return this.rhs.doubleWidth }
	walk(handler: ExpressionHandler) {
		handler(this)
		this.rhs.walk(handler) //don't think there is any need to walk the LHS
	}
	replace(replacements: Map<Expression, Expression>) {
		return new Assignment(
			this.lhs,
			(replacements.get(this.rhs) || this.rhs).replace(replacements)
		)
	}
	toString(omitParens?: boolean) {
		const insideParens = this.lhs.toString() + ' = ' + this.rhs.toString(true) //are parens needed on RHS?
		return omitParens ? insideParens : `(${insideParens})`
	}
}
interface NameReference {
	readonly name: string
}
export class ClassReference extends PrimitiveExpression {
	constructor(public readonly clazz: NameReference) { super() }
	get doubleWidth() { return false }
	toString() { return this.clazz.name }
}
export class FunctionCall extends Expression {
	constructor(
		public readonly obj: Expression,
		public readonly func: NameReference | null, //null if calling a constructor
		public readonly args: Expression[],
		public readonly doubleWidth: boolean
	) { super() }
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
			this.doubleWidth
		)
	}
	toString() {
		const {obj, func, args} = this
		return obj.toString() +
		       (func ? '.' + func.name : '') +
		       `(${args.map(arg => arg.toString(true)).join(', ')})`
	}
}
export class FieldAccess extends Expression {
	constructor(
		public readonly obj: Expression,
		public readonly field: NameReference,
		public readonly doubleWidth: boolean
	) { super() }
	walk(handler: ExpressionHandler) {
		handler(this)
		this.obj.walk(handler)
	}
	replace(replacements: Map<Expression, Expression>) {
		return new FieldAccess(
			(replacements.get(this.obj) || this.obj).replace(replacements),
			this.field,
			this.doubleWidth
		)
	}
	toString() {
		return `${this.obj.toString()}.${this.field.name}`
	}
}
export class ArrayAccess extends Expression {
	constructor(
		public readonly arr: Expression,
		public readonly index: Expression,
		public readonly doubleWidth: boolean
	) { super() }
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
export class NewObject extends Expression {
	public args?: Expression[]
	constructor(public readonly clazz: NameReference) { super() }
	get doubleWidth() { return false }
	walk(handler: ExpressionHandler) {
		handler(this)
		for (const arg of this.args || []) arg.walk(handler)
	}
	replace(replacements: Map<Expression, Expression>) {
		const newExpression = new NewObject(this.clazz)
		if (this.args) {
			newExpression.args = this.args.map(arg =>
				(replacements.get(arg) || arg).replace(replacements)
			)
		}
		return newExpression
	}
	toString() {
		if (!this.args) throw new Error('No arguments given to constructor')
		return `new ${this.clazz.name}` +
		       `(${this.args.map(arg => arg.toString(true)).join(', ')})`
	}
}
export class NewArray extends Expression {
	constructor(
		public readonly type: NameReference,
		public readonly length: Expression,
		public readonly primitive: boolean
	) { super() }
	get doubleWidth() { return false }
	walk(handler: ExpressionHandler) {
		handler(this)
		this.length.walk(handler)
	}
	replace(replacements: Map<Expression, Expression>) {
		return new NewArray(
			this.type,
			(replacements.get(this.length) || this.length).replace(replacements),
			this.primitive
		)
	}
	toString() {
		return `new ${this.type.name}[${this.length.toString(true)}]`
	}
}
export class Cast extends Expression {
	constructor(
		public readonly type: NameReference,
		public readonly exp: Expression,
		public readonly primitive: boolean
	) { super() }
	get doubleWidth() {
		const {name} = this.type
		return name === 'long' || name === 'double'
	}
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
export interface Replacements {
	expressions: Map<Expression, Expression>
	statements: Map<Statement, Statement[]>
}

export abstract class Statement {
	abstract walkExpressions(handler: ExpressionHandler): void
	abstract walkStatements(handler: StatementHandler): void
	abstract replace(replacements: Replacements): Statement
	abstract toSections(enclosingLoop?: LoopReference): Section[]
}
abstract class PrimitiveStatement extends Statement {
	walkStatements(handler: StatementHandler) { handler(this) }
}
export class ExpressionStatement extends PrimitiveStatement {
	constructor(public readonly exp: Expression) { super() }
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
	constructor(public readonly loop: LoopReference) { super() }
	walkExpressions() {}
	replace() { return this }
	toSections(enclosingLoop?: LoopReference) {
		return [`break${enclosingLoop === this.loop ? '' : ' ' + this.loop.label};`]
	}
}
export class ContinueStatement extends PrimitiveStatement {
	constructor(public readonly loop: LoopReference) { super() }
	walkExpressions() {}
	replace() { return this }
	toSections(enclosingLoop?: LoopReference) {
		return [`continue${enclosingLoop === this.loop ? '' : ' ' + this.loop.label};`]
	}
}
export class ReturnStatement extends PrimitiveStatement {
	constructor(public readonly exp: Expression | null) { super() }
	walkExpressions(handler: ExpressionHandler) {
		if (this.exp) this.exp.walk(handler)
	}
	replace({expressions}: Replacements) {
		return new ReturnStatement(
			this.exp
				? (expressions.get(this.exp) || this.exp).replace(expressions)
				: null
		)
	}
	toSections() {
		return [`return${this.exp ? ' ' + this.exp.toString(true) : ''};`]
	}
}
export class ThrowStatement extends PrimitiveStatement {
	constructor(public readonly err: Expression) { super() }
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
export class IfStatement extends Statement {
	constructor(
		public readonly cond: Expression,
		public readonly ifBlock: Block,
		public readonly elseBlock: Block = []
	) { super() }
	walkExpressions(handler: ExpressionHandler) {
		this.cond.walk(handler)
		for (const statement of this.ifBlock) statement.walkExpressions(handler)
		for (const statement of this.elseBlock) statement.walkExpressions(handler)
	}
	walkStatements(handler: StatementHandler) {
		handler(this)
		for (const statement of this.ifBlock) statement.walkStatements(handler)
		for (const statement of this.elseBlock) statement.walkStatements(handler)
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
export class WhileStatement extends Statement {
	constructor(
		public readonly cond: Expression,
		public readonly block: Block,
		public readonly doWhile: boolean,
		public readonly label: LoopReference
	) { super() }
	walkExpressions(handler: ExpressionHandler) {
		this.cond.walk(handler)
		for (const statement of this.block) statement.walkExpressions(handler)
	}
	walkStatements(handler: StatementHandler) {
		handler(this)
		for (const statement of this.block) statement.walkStatements(handler)
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
		const labelString = isLabelNeeded(this) ? this.label.label + ': ' : ''
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
export interface Case {
	exp: Expression | null //null for default
	block: Block
}
export class SwitchStatement extends Statement {
	constructor(
		public readonly val: Expression,
		public readonly cases: Case[],
		public readonly label: LoopReference
	) { super() }
	walkExpressions(handler: ExpressionHandler) {
		this.val.walk(handler)
		for (const {exp, block} of this.cases) {
			if (exp) exp.walk(handler)
			for (const statement of block) statement.walkExpressions(handler)
		}
	}
	walkStatements(handler: StatementHandler) {
		handler(this)
		for (const {block} of this.cases) {
			for (const statement of block) statement.walkStatements(handler)
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
		const labelString = isLabelNeeded(this) ? this.label.label + ': ' : ''
		return [
			`${labelString}switch (${this.val.toString(true)}) {`,
			new IndentedLines(innerSections),
			'}'
		]
	}
}
export type Block = Statement[]
export const flatten = <T>(segments: T[][]): T[] =>
	([] as T[]).concat(...segments)
export function replaceBlock(block: Block, replacements: Replacements): Block {
	const {statements} = replacements
	return flatten(block.map(statement =>
		(statements.get(statement) || [statement]).map(statement =>
			statement.replace(replacements)
		)
	))
}
export const blockToSections = (block: Block, enclosingLoop?: LoopReference) =>
	flatten(block.map(statement => statement.toSections(enclosingLoop)))
export const sectionsToString = (sections: Section[]) =>
	flatten(sections.map(section =>
		section instanceof IndentedLines ? section.toLines() : [section]
	)).join('\n')
export const blockToString = (block: Block) =>
	sectionsToString(blockToSections(block))
function isLabelNeeded(loopOrSwitch: WhileStatement | SwitchStatement) {
	const {label} = loopOrSwitch
	let labelNeeded = false
	loopOrSwitch.walkStatements(statement => { //look for break/continue referencing this loop inside a different loop
		const isLoopOrSwitch = statement instanceof WhileStatement || statement instanceof SwitchStatement
		if (isLoopOrSwitch && statement !== loopOrSwitch) {
			statement.walkStatements(statement => {
				if (statement instanceof BreakStatement || statement instanceof ContinueStatement) {
					if (statement.loop === label) labelNeeded = true
				}
			})
		}
	})
	return labelNeeded
}