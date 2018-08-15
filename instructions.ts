import {
	ArrayAccess,
	Assignment,
	BinaryOp,
	BinaryOperation,
	Block,
	BooleanLiteral,
	Cast,
	ClassLiteral,
	ClassReference,
	Expression,
	ExpressionStatement,
	FieldAccess,
	FloatLiteral,
	FunctionCall,
	IntegerLiteral,
	NewArray,
	NewObject,
	NullLiteral,
	ReturnStatement,
	StringLiteral,
	ThisLiteral,
	ThrowStatement,
	UnaryOp,
	UnaryOperation,
	Variable
} from './ast'
import {Class, LiteralConstant, NameAndType, Ref} from './constant-pool-parser'
import {doubleWidthType, getArgTypes, getType} from './descriptor'
import {LocalType, Primitive, varName} from './variable-types'

export type Stack = Expression[]
export function forcePop(stack: Stack) {
	const stackTop = stack.pop()
	if (!stackTop) throw new Error('Empty stack')
	return stackTop
}

export class MonitorExitError extends Error {
	constructor(public readonly nextInstruction?: number) {
		super('Cannot execute monitorexit')
	}
}

export abstract class Instruction {
	abstract execute(stack: Stack, block: Block): void
	setLocalTypes(_: Map<number, LocalType>) {}
}
export abstract class LoadStoreInstruction extends Instruction {
	constructor(public readonly n: number) { super() }
	abstract readonly type: LocalType
	setLocalTypes(types: Map<number, LocalType>) {
		const {n, type} = this
		const previousType = types.get(n)
		if (previousType) {
			if (previousType !== type) {
				throw new Error(`${varName(n)} used as ${previousType} and ${type}`)
			}
		}
		else types.set(n, type)
	}
}
abstract class LocalLoadInstruction extends LoadStoreInstruction {
	execute(stack: Stack) {
		stack.push(new Variable(
			this.n,
			this.type === 'double' || this.type === 'long'
		))
	}
}
abstract class LocalStoreInstruction extends LoadStoreInstruction {
	execute(stack: Stack, block: Block) {
		block.push(new ExpressionStatement(
			new Assignment(
				new Variable(this.n), //doubleWidth doesn't matter because it's not on the stack
				forcePop(stack)
			)
		))
	}
}
abstract class ArrayLoadInstruction extends Instruction {
	execute(stack: Stack) {
		const index = forcePop(stack),
		      arr = forcePop(stack)
		stack.push(new ArrayAccess(arr, index, false))
	}
}
abstract class ArrayStoreInstruction extends Instruction {
	execute(stack: Stack, block: Block) {
		const value = forcePop(stack),
		      index = forcePop(stack),
		      arr   = forcePop(stack)
		if (arr instanceof NewArray && index instanceof IntegerLiteral) {
			let {elements} = arr
			if (!elements) {
				elements = []
				arr.elements = elements
			}
			elements[index.i] = value
		}
		else {
			block.push(new ExpressionStatement(
				new Assignment(
					new ArrayAccess(arr, index, false),
					value
				)
			))
		}
	}
}
abstract class ReturnInstruction extends Instruction {
	execute(stack: Stack, block: Block) {
		block.push(new ReturnStatement(forcePop(stack)))
	}
}
abstract class InvokeInstruction extends Instruction {
	private readonly argTypes: string[]
	private readonly isVoid: boolean
	private readonly doubleWidth: boolean
	private readonly isBoolean: boolean
	constructor(
		public readonly className: string,
		public readonly method: Ref
	) {
		super()
		const {descriptor} = method.nameAndType
		this.argTypes = getArgTypes(descriptor)
		const returnType = getType(descriptor)
		this.isVoid = returnType === 'void'
		this.doubleWidth = doubleWidthType(returnType)
		this.isBoolean = returnType === 'boolean'
	}
	get isStatic() { return false }
	execute(stack: Stack, block: Block) {
		const {class: clazz, nameAndType} = this.method
		const args = new Array<Expression>(this.argTypes.length)
		for (let i = args.length - 1; i >= 0; i--) args[i] = forcePop(stack)
		const obj = this.isStatic
			? new ClassReference(clazz)
			: forcePop(stack)
		let result: Expression | undefined
		if (nameAndType.name === '<init>') {
			if (obj instanceof NewObject) {
				if (obj.args) throw new Error('Already initialized')
				obj.args = args
			}
			else if (obj instanceof ThisLiteral) {
				const thisObj = new ThisLiteral(clazz.name !== this.className)
				result = new FunctionCall(thisObj, null, args, this.argTypes, false, false)
			}
			else throw new Error('Unexpected <init>() call')
		}
		else result = new FunctionCall(obj, nameAndType, args, this.argTypes, this.doubleWidth, this.isBoolean)
		if (result) {
			if (this.isVoid) block.push(new ExpressionStatement(result))
			else stack.push(result)
		}
	}
}
abstract class NewArrayInstruction extends Instruction {
	abstract readonly name: string
	abstract readonly primitive: boolean
	execute(stack: Stack) {
		stack.push(new NewArray(this, [forcePop(stack)], this.primitive))
	}
}
abstract class FieldInstruction extends Instruction {
	public readonly clazz: Class
	public readonly nameAndType: NameAndType
	protected readonly doubleWidth: boolean
	protected readonly isBoolean: boolean
	constructor({class: clazz, nameAndType}: Ref) {
		super()
		this.clazz = clazz
		this.nameAndType = nameAndType
		const type = getType(nameAndType.descriptor)
		this.doubleWidth = doubleWidthType(type)
		this.isBoolean = type === 'boolean'
	}
}
abstract class InstanceFieldInstruction extends FieldInstruction {
	protected getField(stack: Stack) {
		return new FieldAccess(
			forcePop(stack),
			this.nameAndType,
			this.doubleWidth,
			this.isBoolean
		)
	}
}
abstract class StaticFieldInstruction extends FieldInstruction {
	protected get field() {
		return new FieldAccess(
			new ClassReference(this.clazz),
			this.nameAndType,
			this.doubleWidth,
			this.isBoolean
		)
	}
}
abstract class BinaryOpInstruction extends Instruction {
	abstract readonly op: BinaryOp
	execute(stack: Stack) {
		const value2 = forcePop(stack),
		      value1 = forcePop(stack)
		stack.push(new BinaryOperation(this.op, value1, value2))
	}
}
abstract class AddInstruction extends BinaryOpInstruction {
	get op(): BinaryOp { return '+' }
}
abstract class SubInstruction extends BinaryOpInstruction {
	get op(): BinaryOp { return '-' }
}
abstract class MulInstruction extends BinaryOpInstruction {
	get op(): BinaryOp { return '*' }
}
abstract class DivInstruction extends BinaryOpInstruction {
	get op(): BinaryOp { return '/' }
}
abstract class RemInstruction extends BinaryOpInstruction {
	get op(): BinaryOp { return '%' }
}
abstract class ShiftLeftInstruction extends BinaryOpInstruction {
	get op(): BinaryOp { return '<<' }
}
abstract class ShiftRightInstruction extends BinaryOpInstruction {
	get op(): BinaryOp { return '>>' }
}
abstract class UShiftRightInstruction extends BinaryOpInstruction {
	get op(): BinaryOp { return '>>>' }
}
abstract class AndInstruction extends BinaryOpInstruction {
	get op(): BinaryOp { return '&' }
}
abstract class OrInstruction extends BinaryOpInstruction {
	get op(): BinaryOp { return '|' }
}
abstract class XorInstruction extends BinaryOpInstruction {
	get op(): BinaryOp { return '^' }
}
abstract class UnaryOpInstruction extends Instruction {
	abstract readonly op: UnaryOp
	execute(stack: Stack) {
		stack.push(new UnaryOperation(this.op, forcePop(stack)))
	}
}
abstract class NegInstruction extends UnaryOpInstruction {
	get op(): UnaryOp { return '-' }
}
abstract class PrimitiveCast extends Instruction {
	abstract readonly target: Primitive
	execute(stack: Stack) {
		stack.push(new Cast({name: this.target}, forcePop(stack), true))
	}
}
abstract class IntCast extends PrimitiveCast {
	get target(): Primitive { return 'int' }
}
abstract class LongCast extends PrimitiveCast {
	get target(): Primitive { return 'long' }
}
abstract class FloatCast extends PrimitiveCast {
	get target(): Primitive { return 'float' }
}
abstract class DoubleCast extends PrimitiveCast {
	get target(): Primitive { return 'double' }
}

export class AALoad extends ArrayLoadInstruction {}
export class AAStore extends ArrayStoreInstruction {}
export class ALoad extends LocalLoadInstruction {
	constructor(
		public readonly n: number,
		public readonly isStatic: boolean
	) { super(n) }
	get type(): LocalType { return 'Object' }
	execute(stack: Stack) {
		if (this.n || this.isStatic) super.execute(stack)
		else stack.push(new ThisLiteral)
	}
}
export class ANewArray extends NewArrayInstruction {
	constructor(public readonly clazz: Class) { super() }
	get name() { return this.clazz.name }
	get primitive() { return false }
}
export class AReturn extends ReturnInstruction {}
export class ArrayLength extends Instruction {
	execute(stack: Stack) {
		stack.push(new FieldAccess(
			forcePop(stack),
			{name: 'length'},
			false,
			false
		))
	}
}
export class AStore extends LocalStoreInstruction {
	get type(): LocalType { return 'Object' }
}
export class AThrow extends Instruction {
	execute(stack: Stack, block: Block) {
		const exception = forcePop(stack)
		stack.length = 0
		stack[0] = exception
		block.push(new ThrowStatement(exception))
	}
}
export class BALoad extends ArrayLoadInstruction {}
export class BAStore extends ArrayStoreInstruction {}
export class CALoad extends ArrayLoadInstruction {}
export class CAStore extends ArrayStoreInstruction {}
export class CheckCast extends Instruction {
	public readonly clazz: Class
	constructor(clazz: Class) {
		super()
		const {name} = clazz
		this.clazz = name[0] === '[' ? {name: getType(name)} : clazz
	}
	execute(stack: Stack) {
		stack.push(new Cast(this.clazz, forcePop(stack), false))
	}
}
export class D2F extends FloatCast {}
export class D2I extends IntCast {}
export class D2L extends LongCast {}
export class DConst extends Instruction {
	constructor(public readonly d: number) { super() }
	execute(stack: Stack) {
		stack.push(new FloatLiteral(this.d, true))
	}
}
export class DAdd extends AddInstruction {}
export class DALoad extends ArrayLoadInstruction {}
export class DAStore extends ArrayStoreInstruction {}
export class DCmpL extends SubInstruction {} //not sure how to distinguish how they deal with NaN
export class DCmpG extends SubInstruction {}
export class DDiv extends DivInstruction {}
export class DLoad extends LocalLoadInstruction {
	get type(): LocalType { return 'double' }
}
export class DMul extends MulInstruction {}
export class DNeg extends NegInstruction {}
export class DRem extends RemInstruction {}
export class DReturn extends ReturnInstruction {}
export class DStore extends LocalStoreInstruction {
	get type(): LocalType { return 'double' }
}
export class DSub extends SubInstruction {}
export class Dup extends Instruction {
	execute(stack: Stack) {
		const val = forcePop(stack)
		stack.push(val, val)
	}
}
export class F2D extends DoubleCast {}
export class F2I extends IntCast {}
export class F2L extends LongCast {}
export class FAdd extends AddInstruction {}
export class FALoad extends ArrayLoadInstruction {}
export class FAStore extends ArrayStoreInstruction {}
export class FCmpL extends SubInstruction {} //not sure how to distinguish how they deal with NaN
export class FCmpG extends SubInstruction {}
export class FConst extends Instruction {
	constructor(public readonly f: number) { super() }
	execute(stack: Stack) {
		stack.push(new FloatLiteral(this.f))
	}
}
export class FDiv extends DivInstruction {}
export class FLoad extends LocalLoadInstruction {
	get type(): LocalType { return 'float' }
}
export class FMul extends MulInstruction {}
export class FNeg extends NegInstruction {}
export class FRem extends RemInstruction {}
export class FReturn extends ReturnInstruction {}
export class FStore extends LocalStoreInstruction {
	get type(): LocalType { return 'float' }
}
export class FSub extends SubInstruction {}
export class GetField extends InstanceFieldInstruction {
	execute(stack: Stack) {
		stack.push(this.getField(stack))
	}
}
export class GetStatic extends StaticFieldInstruction {
	execute(stack: Stack) {
		stack.push(this.field)
	}
}
export class I2B extends PrimitiveCast {
	get target(): Primitive { return 'byte' }
}
export class I2C extends PrimitiveCast {
	get target(): Primitive { return 'char' }
}
export class I2D extends DoubleCast {}
export class I2F extends FloatCast {}
export class I2L extends LongCast {}
export class I2S extends PrimitiveCast {
	get target(): Primitive { return 'short' }
}
export class IAdd extends AddInstruction {}
export class IALoad extends ArrayLoadInstruction {}
export class IAnd extends AndInstruction {}
export class IAStore extends ArrayStoreInstruction {}
export class IConst extends Instruction {
	constructor(public readonly i: number) { super() }
	execute(stack: Stack) {
		stack.push(new IntegerLiteral(this.i))
	}
}
export class IDiv extends DivInstruction {}
export class IInc extends Instruction {
	constructor(
		public readonly n: number,
		public readonly i: number
	) { super() }
	execute(_: Stack, block: Block) {
		const variable = new Variable(this.n, false)
		block.push(new ExpressionStatement(
			new Assignment(
				variable,
				new BinaryOperation('+', variable, new IntegerLiteral(this.i))
			)
		))
	}
}
export class ILoad extends LocalLoadInstruction {
	get type(): LocalType { return 'int' }
}
export class IMul extends MulInstruction {}
export class INeg extends NegInstruction {}
export class InstanceOf extends Instruction {
	constructor(public readonly clazz: Class) { super() }
	execute(stack: Stack) {
		stack.push(new BinaryOperation(
			'instanceof',
			forcePop(stack),
			new ClassReference(this.clazz)
		))
	}
}
export class IOr extends OrInstruction {}
export class IRem extends RemInstruction {}
export class IReturn extends ReturnInstruction {}
export class IShiftLeft extends ShiftLeftInstruction {}
export class IShiftRight extends ShiftRightInstruction {}
export class IStore extends LocalStoreInstruction {
	get type(): LocalType { return 'int' }
}
export class ISub extends SubInstruction {}
export class IUShiftRight extends UShiftRightInstruction {}
export class IXor extends XorInstruction {}
export class InvokeInterface extends InvokeInstruction {}
export class InvokeSpecial extends InvokeInstruction {}
export class InvokeStatic extends InvokeInstruction {
	get isStatic() { return true }
}
export class InvokeVirtual extends InvokeInstruction {}
export class L2D extends DoubleCast {}
export class L2F extends FloatCast {}
export class L2I extends IntCast {}
export class LAdd extends AddInstruction {}
export class LALoad extends ArrayLoadInstruction {}
export class LAnd extends AndInstruction {}
export class LAStore extends ArrayStoreInstruction {}
export class LCmp extends SubInstruction {} //not precisely true, but this seems to be how javac uses it
export class LConst extends Instruction {
	constructor(public readonly l: BigInt) { super() }
	execute(stack: Stack) {
		stack.push(new IntegerLiteral(this.l, true))
	}
}
export class LDiv extends DivInstruction {}
export class LLoad extends LocalLoadInstruction {
	get type(): LocalType { return 'long' }
}
export class LMul extends MulInstruction {}
export class LNeg extends NegInstruction {}
export class LOr extends OrInstruction {}
export class LRem extends RemInstruction {}
export class LReturn extends ReturnInstruction {}
export class LShiftLeft extends ShiftLeftInstruction {}
export class LShiftRight extends ShiftRightInstruction {}
export class LStore extends LocalStoreInstruction {
	get type(): LocalType { return 'long' }
}
export class LSub extends SubInstruction {}
export class LUShiftRight extends UShiftRightInstruction {}
export class LXor extends XorInstruction {}
export class LoadConstant extends Instruction {
	constructor(public readonly constant: LiteralConstant) { super() }
	execute(stack: Stack) {
		let exp: Expression
		switch (this.constant.type) {
			case 'class':
				exp = new ClassLiteral(this.constant)
				break
			case 'string':
				exp = new StringLiteral(this.constant.value)
				break
			case 'int':
				exp = new IntegerLiteral(this.constant.value)
				break
			case 'long':
				exp = new IntegerLiteral(this.constant.value, true)
				break
			case 'float':
				exp = new FloatLiteral(this.constant.value)
				break
			case 'double':
				exp = new FloatLiteral(this.constant.value, true)
				break
			default:
				throw new Error('Unexpected literal type: ' + (this.constant as LiteralConstant).type)
		}
		stack.push(exp)
	}
}
export class MonitorEnter extends Instruction {
	execute() { throw new Error('Cannot execute monitorenter') }
}
export class MonitorExit extends Instruction {
	execute() { throw new MonitorExitError }
}
export class MultiANewArray extends Instruction {
	constructor(
		public readonly clazz: Class,
		public readonly dimensions: number
	) {
		super()
		const name = getType(clazz.name)
		const brackets = '[]'.repeat(dimensions)
		if (!name.endsWith(brackets)) throw new Error('Array type has wrong dimensionality')
		clazz.name = name.slice(0, -brackets.length)
	}
	execute(stack: Stack) {
		const {dimensions} = this
		const args = new Array<Expression>(dimensions)
		for (let i = dimensions - 1; i >= 0; i--) args[i] = forcePop(stack)
		stack.push(new NewArray(this.clazz, args, false))
	}
}
export class New extends Instruction {
	constructor(public readonly clazz: Class) { super() }
	execute(stack: Stack) {
		stack.push(new NewObject(this.clazz))
	}
}
export class NewPrimitiveArray extends NewArrayInstruction {
	constructor(public readonly name: Primitive) { super() }
	get primitive() { return true }
}
export class NoOp extends Instruction {
	execute() {}
}
export class NullConst extends Instruction {
	execute(stack: Stack) {
		stack.push(new NullLiteral)
	}
}
export class Pop extends Instruction {
	constructor(public readonly n: number) { super() }
	execute(stack: Stack, block: Block) {
		let popped = 0
		const expressions: Stack = []
		while (popped < this.n) {
			const top = forcePop(stack)
			expressions.push(top)
			popped += top.doubleWidth ? 2 : 1
		}
		if (popped > this.n) throw new Error('Popped too much')
		// Push expressions in order they were pushed onto stack
		for (let i = expressions.length - 1; i >= 0; i--) {
			block.push(new ExpressionStatement(expressions[i]))
		}
	}
}
export class PutField extends InstanceFieldInstruction {
	execute(stack: Stack, block: Block) {
		const value = forcePop(stack)
		block.push(new ExpressionStatement(
			new Assignment(
				this.getField(stack),
				value
			)
		))
	}
}
export class PutStatic extends StaticFieldInstruction {
	execute(stack: Stack, block: Block) {
		block.push(new ExpressionStatement(
			new Assignment(
				this.field,
				forcePop(stack)
			)
		))
	}
}
export class Return extends Instruction {
	execute(_: Stack, block: Block) {
		block.push(new ReturnStatement)
	}
}
export class SALoad extends ArrayLoadInstruction {}
export class SAStore extends ArrayStoreInstruction {}
export class SPush extends Instruction {
	constructor(public readonly i: number) { super() }
	execute(stack: Stack) {
		stack.push(new IntegerLiteral(this.i))
	}
}
//Used for both tableswitch and lookupswitch
//Doesn't function much like a simple jump, so not subclassed from Jump
export class SwitchInstruction extends Instruction {
	constructor(
		public readonly offsetMap: Map<number, number>,
		public readonly defaultOffset: number
	) { super() }
	execute() { throw new Error('Cannot execute tableswitch or lookupswitch') }
}
//When executed, jumps push the expression under which they jump onto the stack
export abstract class Jump extends Instruction {
	constructor(public readonly offset: number) { super() }
	abstract readonly offsets: Iterable<number>
}
export abstract class IfCondition extends Jump {
	protected abstract readonly jumpOp: BinaryOp
	get offsets() { return [3, this.offset] } //if* instruction is 3 bytes long
}
abstract class IfICmp extends IfCondition {
	execute(stack: Stack) {
		const value2 = forcePop(stack),
		      value1 = forcePop(stack)
		stack.push(new BinaryOperation(this.jumpOp, value1, value2))
	}
}
export class IfACmpEq extends IfICmp {
	get jumpOp(): BinaryOp { return '==' }
}
export class IfACmpNe extends IfICmp {
	get jumpOp(): BinaryOp { return '!=' }
}
export class IfICmpEq extends IfICmp {
	get jumpOp(): BinaryOp { return '==' }
}
export class IfICmpNe extends IfICmp {
	get jumpOp(): BinaryOp { return '!=' }
}
export class IfICmpLt extends IfICmp {
	get jumpOp(): BinaryOp { return '<' }
}
export class IfICmpGe extends IfICmp {
	get jumpOp(): BinaryOp { return '>=' }
}
export class IfICmpGt extends IfICmp {
	get jumpOp(): BinaryOp { return '>' }
}
export class IfICmpLe extends IfICmp {
	get jumpOp(): BinaryOp { return '<=' }
}
abstract class If extends IfCondition {
	execute(stack: Stack) {
		stack.push(new BinaryOperation(
			this.jumpOp,
			forcePop(stack),
			new IntegerLiteral(0)
		))
	}
}
export class IfEq extends If {
	get jumpOp(): BinaryOp { return '==' }
}
export class IfNe extends If {
	get jumpOp(): BinaryOp { return '!=' }
}
export class IfLt extends If {
	get jumpOp(): BinaryOp { return '<' }
}
export class IfGe extends If {
	get jumpOp(): BinaryOp { return '>=' }
}
export class IfGt extends If {
	get jumpOp(): BinaryOp { return '>' }
}
export class IfLe extends If {
	get jumpOp(): BinaryOp { return '<=' }
}
abstract class IfNullCmp extends IfCondition {
	execute(stack: Stack) {
		stack.push(new BinaryOperation(
			this.jumpOp,
			forcePop(stack),
			new NullLiteral
		))
	}
}
export class IfNull extends IfNullCmp {
	get jumpOp(): BinaryOp { return '==' }
}
export class IfNonNull extends IfNullCmp {
	get jumpOp(): BinaryOp { return '!=' }
}
export class Goto extends Jump {
	get offsets() { return [this.offset] }
	execute(stack: Stack) {
		stack.push(new BooleanLiteral(true))
	}
}