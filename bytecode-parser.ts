import {Parser, parseInt, parseByteArray, parseAndThen, parseReturn, parseTimes, slice} from './parse'
import {
	ArrayAccess,
	Assignment,
	BinaryOp,
	BinaryOperation,
	Block,
	BooleanLiteral,
	Cast,
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
import {constantParser} from './constant-parser'
import ConstantPoolIndex from './constant-pool-index'
import {ConstantPool, Class, LiteralConstant, NameAndType, Ref} from './constant-pool-parser'

const LDC = 0x12

export type Stack = Expression[]
export function forcePop(stack: Stack) {
	const stackTop = stack.pop()
	if (!stackTop) throw new Error('Empty stack')
	return stackTop
}

function argCount({descriptor}: NameAndType) {
	let count = 0
	charLoop: for (let i = 1; i < descriptor.length; i++) { //skip leading open paren
		switch (descriptor[i]) {
			case 'L':
				while (descriptor[++i] !== ';') {
					if (i > descriptor.length) throw new Error('Missing semicolon in descriptor')
				}
				count++
				break
			case 'B':
			case 'C':
			case 'D':
			case 'F':
			case 'I':
			case 'J':
			case 'S':
			case 'Z':
				count++
				break
			case '[':
				break
			case ')':
				break charLoop
			default:
				throw new Error('Unexpected character in descriptor: ' + descriptor[i])
		}
	}
	return count
}
function isDoubleWidth({descriptor}: NameAndType) {
	const returnType = descriptor.substring(descriptor.indexOf(')') + 1)
	return returnType === 'J' || returnType === 'D'
}

export abstract class Operation {
	abstract execute(stack: Stack, block: Block): void
}
abstract class LocalLoadOperation extends Operation {
	constructor(public readonly n: number) { super() }
	abstract readonly doubleWidth: boolean
	execute(stack: Stack) {
		stack.push(new Variable(this.n, this.doubleWidth))
	}
}
abstract class LocalStoreOperation extends Operation {
	constructor(public readonly n: number) { super() }
	execute(stack: Stack, block: Block) {
		block.push(new ExpressionStatement(
			new Assignment(
				new Variable(this.n), //doubleWidth doesn't matter because it's not on the stack
				forcePop(stack)
			)
		))
	}
}
abstract class ArrayLoadOperation extends Operation {
	execute(stack: Stack) {
		const index = forcePop(stack)
		const arr = forcePop(stack)
		stack.push(new ArrayAccess(arr, index, false))
	}
}
abstract class ArrayStoreOperation extends Operation {
	execute(stack: Stack, block: Block) {
		const value = forcePop(stack)
		const index = forcePop(stack)
		const arr = forcePop(stack)
		block.push(new ExpressionStatement(
			new Assignment(
				new ArrayAccess(arr, index, false),
				value
			)
		))
	}
}
abstract class ReturnOperation extends Operation {
	execute(stack: Stack, block: Block) {
		block.push(new ReturnStatement(forcePop(stack)))
	}
}
abstract class InvokeOperation extends Operation {
	constructor(public readonly method: Ref) { super() }
	get isStatic() { return false }
	execute(stack: Stack) {
		const {nameAndType} = this.method
		const argsLength = argCount(nameAndType)
		const args = new Array<Expression>(argsLength)
		for (let i = argsLength - 1; i >= 0; i--) args[i] = forcePop(stack)
		const obj = this.isStatic
			? new ClassReference(this.method.class)
			: forcePop(stack)
		if (nameAndType.name === '<init>' && obj instanceof NewObject && !obj.args) {
			obj.args = args
		}
		else {
			const call = new FunctionCall(obj, nameAndType, args, isDoubleWidth(nameAndType))
			stack.push(call)
		}
	}
}
abstract class NewArrayOperation extends Operation {
	abstract readonly name: string
	abstract readonly primitive: boolean
	execute(stack: Stack) {
		stack.push(new NewArray(this, forcePop(stack), this.primitive))
	}
}
abstract class BinaryOpOperation extends Operation {
	abstract readonly op: BinaryOp
	execute(stack: Stack) {
		const value2 = forcePop(stack),
		      value1 = forcePop(stack)
		stack.push(new BinaryOperation(this.op, value1, value2))
	}
}
abstract class AddOperation extends BinaryOpOperation {
	get op(): BinaryOp { return '+' }
}
abstract class SubOperation extends BinaryOpOperation {
	get op(): BinaryOp { return '-' }
}
abstract class MulOperation extends BinaryOpOperation {
	get op(): BinaryOp { return '*' }
}
abstract class DivOperation extends BinaryOpOperation {
	get op(): BinaryOp { return '/' }
}
abstract class RemOperation extends BinaryOpOperation {
	get op(): BinaryOp { return '%' }
}
abstract class ShiftLeftOpertaion extends BinaryOpOperation {
	get op(): BinaryOp { return '<<' }
}
abstract class ShiftRightOperation extends BinaryOpOperation {
	get op(): BinaryOp { return '>>' }
}
abstract class UShiftRightOperation extends BinaryOpOperation {
	get op(): BinaryOp { return '>>>' }
}
abstract class AndOperation extends BinaryOpOperation {
	get op(): BinaryOp { return '&' }
}
abstract class OrOperation extends BinaryOpOperation {
	get op(): BinaryOp { return '|' }
}
abstract class XorOperation extends BinaryOpOperation {
	get op(): BinaryOp { return '^' }
}
abstract class UnaryOpOperation extends Operation {
	abstract readonly op: UnaryOp
	execute(stack: Stack) {
		stack.push(new UnaryOperation(this.op, forcePop(stack)))
	}
}
abstract class NegOperation extends UnaryOpOperation {
	get op(): UnaryOp { return '-' }
}
abstract class PrimitiveCast extends Operation {
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

class AALoad extends ArrayLoadOperation {}
class AAStore extends ArrayStoreOperation {}
class ALoad extends LocalLoadOperation {
	constructor(
		public readonly n: number,
		public readonly isStatic: boolean
	) { super(n) }
	get doubleWidth() { return false }
	execute(stack: Stack) {
		if (this.n || this.isStatic) super.execute(stack)
		else stack.push(new ThisLiteral)
	}
}
class ANewArray extends NewArrayOperation {
	constructor(public readonly clazz: Class) { super() }
	get name() { return this.clazz.name }
	get primitive() { return false }
}
class AReturn extends ReturnOperation {}
class ArrayLength extends Operation {
	execute(stack: Stack) {
		stack.push(new FieldAccess(
			forcePop(stack),
			{name: 'length'},
			false
		))
	}
}
class AStore extends LocalStoreOperation {}
class AThrow extends Operation {
	execute(stack: Stack, block: Block) {
		const exception = forcePop(stack)
		stack.length = 0
		stack[0] = exception
		block.push(new ThrowStatement(exception))
	}
}
class BALoad extends ArrayLoadOperation {}
class BAStore extends ArrayStoreOperation {}
class CALoad extends ArrayLoadOperation {}
class CAStore extends ArrayStoreOperation {}
class CheckCast extends Operation {
	constructor(public readonly clazz: Class) { super() }
	execute(stack: Stack) {
		stack.push(new Cast(this.clazz, forcePop(stack), false))
	}
}
class D2F extends FloatCast {}
class D2I extends IntCast {}
class D2L extends LongCast {}
class DConst extends Operation {
	constructor(public readonly d: number) { super() }
	execute(stack: Stack) {
		stack.push(new FloatLiteral(this.d, true))
	}
}
class DAdd extends AddOperation {}
class DALoad extends ArrayLoadOperation {}
class DAStore extends ArrayStoreOperation {}
class DCmpL extends SubOperation {} //not sure how to distinguish how they deal with NaN
class DCmpG extends SubOperation {}
class DDiv extends DivOperation {}
class DLoad extends LocalLoadOperation {
	get doubleWidth() { return true }
}
class DMul extends MulOperation {}
class DNeg extends NegOperation {}
class DRem extends RemOperation {}
class DReturn extends ReturnOperation {}
class DStore extends LocalStoreOperation {}
class DSub extends SubOperation {}
class Dup extends Operation {
	execute(stack: Stack) {
		const val = forcePop(stack)
		stack.push(val, val)
	}
}
class F2D extends DoubleCast {}
class F2I extends IntCast {}
class F2L extends LongCast {}
class FAdd extends AddOperation {}
class FALoad extends ArrayLoadOperation {}
class FAStore extends ArrayStoreOperation {}
class FCmpL extends SubOperation {} //not sure how to distinguish how they deal with NaN
class FCmpG extends SubOperation {}
class FConst extends Operation {
	constructor(public readonly f: number) { super() }
	execute(stack: Stack) {
		stack.push(new FloatLiteral(this.f))
	}
}
class FDiv extends DivOperation {}
class FLoad extends LocalLoadOperation {
	get doubleWidth() { return false }
}
class FMul extends MulOperation {}
class FNeg extends NegOperation {}
class FRem extends RemOperation {}
class FReturn extends ReturnOperation {}
class FStore extends LocalStoreOperation {}
class FSub extends SubOperation {}
class GetStatic extends Operation {
	constructor(public readonly field: Ref) { super() }
	execute(stack: Stack) {
		const {nameAndType} = this.field
		stack.push(new FieldAccess(
			new ClassReference(this.field.class),
			nameAndType,
			isDoubleWidth(nameAndType)
		))
	}
}
class I2B extends PrimitiveCast {
	get target(): Primitive { return 'byte' }
}
class I2C extends PrimitiveCast {
	get target(): Primitive { return 'char' }
}
class I2D extends DoubleCast {}
class I2F extends FloatCast {}
class I2L extends LongCast {}
class I2S extends PrimitiveCast {
	get target(): Primitive { return 'short' }
}
class IAdd extends AddOperation {}
class IALoad extends ArrayLoadOperation {}
class IAnd extends AndOperation {}
class IAStore extends ArrayStoreOperation {}
class IConst extends Operation {
	constructor(public readonly i: number) { super() }
	execute(stack: Stack) {
		stack.push(new IntegerLiteral(this.i))
	}
}
class IDiv extends DivOperation {}
class IInc extends Operation {
	constructor(
		public readonly n: number,
		public readonly i: number
	) { super() }
	execute(_: Stack, block: Block) {
		const variable = new Variable(this.n, false)
		block.push(new ExpressionStatement(
			this.i === 1
				? new UnaryOperation({op: '++', post: true}, variable)
				: new Assignment(
						variable,
						new BinaryOperation('+', variable, new IntegerLiteral(this.i))
					)
		))
	}
}
class ILoad extends LocalLoadOperation {
	get doubleWidth() { return false }
}
class IMul extends MulOperation {}
class INeg extends NegOperation {}
class IOr extends OrOperation {}
class IRem extends RemOperation {}
class IReturn extends ReturnOperation {}
class IShiftLeft extends ShiftLeftOpertaion {}
class IShiftRight extends ShiftRightOperation {}
class IStore extends LocalStoreOperation {}
class ISub extends SubOperation {}
class IUShiftRight extends UShiftRightOperation {}
class IXor extends XorOperation {}
class InvokeInterface extends InvokeOperation {}
class InvokeSpecial extends InvokeOperation {}
class InvokeStatic extends InvokeOperation {
	get isStatic() { return true }
}
class InvokeVirtual extends InvokeOperation {}
class L2D extends DoubleCast {}
class L2F extends FloatCast {}
class L2I extends IntCast {}
class LAdd extends AddOperation {}
class LALoad extends ArrayLoadOperation {}
class LAnd extends AndOperation {}
class LAStore extends ArrayStoreOperation {}
class LCmp extends SubOperation {} //not precisely true, but this seems to be how javac uses it
class LConst extends Operation {
	constructor(public readonly l: number) { super() }
	execute(stack: Stack) {
		stack.push(new IntegerLiteral(this.l, true))
	}
}
class LDiv extends DivOperation {}
class LLoad extends LocalLoadOperation {
	get doubleWidth() { return true }
}
class LMul extends MulOperation {}
class LNeg extends NegOperation {}
class LOr extends OrOperation {}
class LRem extends RemOperation {}
class LReturn extends ReturnOperation {}
class LShiftLeft extends ShiftLeftOpertaion {}
class LShiftRight extends ShiftRightOperation {}
class LStore extends LocalStoreOperation {}
class LSub extends SubOperation {}
class LUShiftRight extends UShiftRightOperation {}
class LXor extends XorOperation {}
class LoadConstant extends Operation {
	constructor(public readonly constant: LiteralConstant) { super() }
	execute(stack: Stack) {
		const {type, value} = this.constant
		let exp: Expression
		switch (type) {
			case 'string':
				exp = new StringLiteral(value as string)
				break
			case 'int':
				exp = new IntegerLiteral(value as number)
				break
			case 'long':
				exp = new IntegerLiteral(value as number, true)
				break
			case 'float':
				exp = new FloatLiteral(value as number)
				break
			case 'double':
				exp = new FloatLiteral(value as number, true)
				break
			default:
				throw new Error('Unexpected literal type: ' + type)
		}
		stack.push(exp)
	}
}
class New extends Operation {
	constructor(public readonly clazz: Class) { super() }
	execute(stack: Stack) {
		stack.push(new NewObject(this.clazz))
	}
}
type Primitive
	= 'boolean'
	| 'char'
	| 'float'
	| 'double'
	| 'byte'
	| 'short'
	| 'int'
	| 'long'
class NewPrimitiveArray extends NewArrayOperation {
	constructor(public readonly name: Primitive) { super() }
	get primitive() { return true }
}
class NullConst extends Operation {
	execute(stack: Stack) {
		stack.push(new NullLiteral)
	}
}
class Pop extends Operation {
	constructor(public readonly n: number) { super() }
	execute(stack: Stack, block: Block) {
		let popped = 0
		const expressions: Stack = []
		while (popped < this.n) {
			const top = forcePop(stack)
			expressions.push(top)
			popped++
			if (top.doubleWidth) popped++
		}
		if (popped > this.n) throw new Error('Popped too much')
		// Push expressions in order they were pushed onto stack
		for (let i = expressions.length - 1; i >= 0; i--) {
			block.push(new ExpressionStatement(expressions[i]))
		}
	}
}
class Return extends Operation {
	execute(_: Stack, block: Block) {
		block.push(new ReturnStatement(null))
	}
}
class SALoad extends ArrayLoadOperation {}
class SAStore extends ArrayStoreOperation {}
class SPush extends Operation {
	constructor(public readonly i: number) { super() }
	execute(stack: Stack) {
		stack.push(new IntegerLiteral(this.i, false))
	}
}
export class TableSwitch extends Operation { //doesn't function much like a simple jump, so not subclassed from Jump
	constructor(
		public readonly offsetMap: Map<number, number>,
		public readonly defaultOffset: number
	) { super() }
	execute() { throw new Error('Cannot execute tableswitch') }
}
//When executed, jumps push the expression under which they jump onto the stack
export abstract class Jump extends Operation {
	abstract readonly offsets: Iterable<number>
}
export abstract class IfICmp extends Jump {
	constructor(public readonly offset: number) { super() }
	protected abstract readonly jumpOp: BinaryOp
	get offsets() { return [3, this.offset] } //if_icmp* instruction is 3 bytes long
	execute(stack: Stack) {
		const value2 = forcePop(stack), value1 = forcePop(stack)
		stack.push(new BinaryOperation(this.jumpOp, value1, value2))
	}
}
class IfACmpEq extends IfICmp {
	get jumpOp(): BinaryOp { return '==' }
}
class IfACmpNe extends IfICmp {
	get jumpOp(): BinaryOp { return '!=' }
}
class IfICmpEq extends IfICmp {
	get jumpOp(): BinaryOp { return '==' }
}
class IfICmpNe extends IfICmp {
	get jumpOp(): BinaryOp { return '!=' }
}
class IfICmpLt extends IfICmp {
	get jumpOp(): BinaryOp { return '<' }
}
class IfICmpGe extends IfICmp {
	get jumpOp(): BinaryOp { return '>=' }
}
class IfICmpGt extends IfICmp {
	get jumpOp(): BinaryOp { return '>' }
}
class IfICmpLe extends IfICmp {
	get jumpOp(): BinaryOp { return '<=' }
}
export abstract class If extends Jump {
	constructor(public readonly offset: number) { super() }
	protected abstract readonly jumpOp: BinaryOp
	get offsets() { return [3, this.offset] } //if* instruction is 3 bytes long
	execute(stack: Stack) {
		stack.push(new BinaryOperation(
			this.jumpOp,
			forcePop(stack),
			new IntegerLiteral(0)
		))
	}
}
class IfEq extends If {
	get jumpOp(): BinaryOp { return '==' }
}
class IfNe extends If {
	get jumpOp(): BinaryOp { return '!=' }
}
class IfLt extends If {
	get jumpOp(): BinaryOp { return '<' }
}
class IfGe extends If {
	get jumpOp(): BinaryOp { return '>=' }
}
class IfGt extends If {
	get jumpOp(): BinaryOp { return '>' }
}
class IfLe extends If {
	get jumpOp(): BinaryOp { return '<=' }
}
export class Goto extends Jump {
	constructor(public readonly offset: number) { super() }
	get offsets() { return [this.offset] }
	execute(stack: Stack) {
		stack.push(new BooleanLiteral(true))
	}
}

interface InstructionLength {
	instruction: Operation
	length: number
}
export type Code = Map<number, InstructionLength>

interface LoadStoreConstructor {
	new(index: number, isStatic: boolean): LocalLoadOperation | LocalStoreOperation
}
function singleByteIndex(data: DataView, offset: number) {
	const index = data.getUint8(offset)
	return {index, newOffset: offset + 1}
}
function doubleByteIndex(data: DataView, offset: number) {
	const index = data.getUint16(offset)
	return {index, newOffset: offset + 2}
}
function localOperation(data: DataView, offset: number, clazz: LoadStoreConstructor, isStatic?: boolean) {
	const {index, newOffset} = singleByteIndex(data, offset)
	return {instruction: new clazz(index, isStatic!), newOffset}
}
interface JumpConstructor {
	new(offset: number): Jump
}
function jumpOperation(data: DataView, offset: number, clazz: JumpConstructor) {
	const {result: jumpOffset, length} = parseSignedShort(data, offset)
	return {instruction: new clazz(jumpOffset), newOffset: offset + length}
}
const parseSignedByte = (data: DataView, offset: number) =>
	({result: data.getInt8(offset), length: 1})
const parseSignedShort = (data: DataView, offset: number) =>
	({result: data.getInt16(offset), length: 2})
const parseSignedInt = parseAndThen(parseInt, int => parseReturn(int | 0))
const parseTableSwitch: Parser<TableSwitch> =
	parseAndThen(parseSignedInt, defaultOffset =>
		parseAndThen(parseSignedInt, low =>
			parseAndThen(parseSignedInt, high =>
				parseAndThen(
					parseTimes(parseSignedInt, high - low + 1),
					jumpOffsets => {
						const offsetMap = new Map<number, number>()
						for (let i = low; i <= high; i++) offsetMap.set(i, jumpOffsets[i - low])
						return parseReturn(new TableSwitch(offsetMap, defaultOffset))
					}
				)
			)
		)
	)

const bytecodesFound = new Set<number>()

const parseCode = parseByteArray(parseInt)
export const bytecodeParser: (constantPool: ConstantPool, isStatic: boolean) => Parser<Code> =
	(constantPool, isStatic) => parseAndThen(parseCode, bytecode => {
		const data = new DataView(bytecode)
		const instructions: Code = new Map
		let offset = 0
		while (offset < bytecode.byteLength) {
			const opCodeOffset = offset
			let opCode = data.getUint8(offset++)
			bytecodesFound.add(opCode)
			let wide: boolean
			if (opCode === 0xc4) { //wide
				wide = true
				opCode = data.getUint8(offset++)
				//TODO: implement wide processing for other instructions
			}
			else if (opCode === 0x13) { //wide load const
				wide = true
				opCode = LDC
			}
			else wide = false
			let instruction: Operation
			let newOffset: number | undefined
			switch (opCode) {
				case 0x01:
					instruction = new NullConst
					break
				case 0x02:
					instruction = new IConst(-1)
					break
				case 0x03:
					instruction = new IConst(0)
					break
				case 0x04:
					instruction = new IConst(1)
					break
				case 0x05:
					instruction = new IConst(2)
					break
				case 0x06:
					instruction = new IConst(3)
					break
				case 0x07:
					instruction = new IConst(4)
					break
				case 0x08:
					instruction = new IConst(5)
					break
				case 0x09:
					instruction = new LConst(0)
					break
				case 0x0a:
					instruction = new LConst(1)
					break
				case 0x0b:
					instruction = new FConst(0)
					break
				case 0x0c:
					instruction = new FConst(1)
					break
				case 0x0d:
					instruction = new FConst(2)
					break
				case 0x0e:
					instruction = new DConst(0)
					break
				case 0x0f:
					instruction = new DConst(1)
					break
				case 0x10: {
					const value = data.getInt8(offset)
					offset++
					instruction = new IConst(value)
					break
				}
				case 0x11: {
					const {result: value, length} = parseSignedShort(data, offset)
					instruction = new SPush(value)
					offset += length
					break
				}
				case LDC: {
					const index = (wide ? doubleByteIndex : singleByteIndex)(data, offset)
					;({newOffset} = index)
					const constant = new ConstantPoolIndex<LiteralConstant>(index.index)
						.getValue(constantPool)
					instruction = new LoadConstant(constant)
					break
				}
				case 0x14: {
					const {result: constant, length} = constantParser(slice(data, offset))
					instruction = new LoadConstant(constant.getValue(constantPool))
					offset += length
					break
				}
				case 0x15:
					({instruction, newOffset} = localOperation(data, offset, ILoad))
					break
				case 0x16:
					({instruction, newOffset} = localOperation(data, offset, LLoad))
					break
				case 0x17:
					({instruction, newOffset} = localOperation(data, offset, FLoad))
					break
				case 0x18:
					({instruction, newOffset} = localOperation(data, offset, DLoad))
					break
				case 0x19:
					({instruction, newOffset} = localOperation(data, offset, ALoad, isStatic))
					break
				case 0x1a:
					instruction = new ILoad(0)
					break
				case 0x1b:
					instruction = new ILoad(1)
					break
				case 0x1c:
					instruction = new ILoad(2)
					break
				case 0x1d:
					instruction = new ILoad(3)
					break
				case 0x1e:
					instruction = new LLoad(0)
					break
				case 0x1f:
					instruction = new LLoad(1)
					break
				case 0x20:
					instruction = new LLoad(2)
					break
				case 0x21:
					instruction = new LLoad(3)
					break
				case 0x22:
					instruction = new FLoad(0)
					break
				case 0x23:
					instruction = new FLoad(1)
					break
				case 0x24:
					instruction = new FLoad(2)
					break
				case 0x25:
					instruction = new FLoad(3)
					break
				case 0x26:
					instruction = new DLoad(0)
					break
				case 0x27:
					instruction = new DLoad(1)
					break
				case 0x28:
					instruction = new DLoad(2)
					break
				case 0x29:
					instruction = new DLoad(3)
					break
				case 0x2a:
					instruction = new ALoad(0, isStatic)
					break
				case 0x2b:
					instruction = new ALoad(1, isStatic)
					break
				case 0x2c:
					instruction = new ALoad(2, isStatic)
					break
				case 0x2d:
					instruction = new ALoad(3, isStatic)
					break
				case 0x2e:
					instruction = new IALoad
					break
				case 0x2f:
					instruction = new LALoad
					break
				case 0x30:
					instruction = new FALoad
					break
				case 0x31:
					instruction = new DALoad
					break
				case 0x32:
					instruction = new AALoad
					break
				case 0x33:
					instruction = new BALoad
					break
				case 0x34:
					instruction = new CALoad
					break
				case 0x35:
					instruction = new SALoad
					break
				case 0x36:
					({instruction, newOffset} = localOperation(data, offset, IStore))
					break
				case 0x37:
					({instruction, newOffset} = localOperation(data, offset, LStore))
					break
				case 0x38:
					({instruction, newOffset} = localOperation(data, offset, FStore))
					break
				case 0x39:
					({instruction, newOffset} = localOperation(data, offset, DStore))
					break
				case 0x3a:
					({instruction, newOffset} = localOperation(data, offset, AStore))
					break
				case 0x3b:
					instruction = new IStore(0)
					break
				case 0x3c:
					instruction = new IStore(1)
					break
				case 0x3d:
					instruction = new IStore(2)
					break
				case 0x3e:
					instruction = new IStore(3)
					break
				case 0x3f:
					instruction = new LStore(0)
					break
				case 0x40:
					instruction = new LStore(1)
					break
				case 0x41:
					instruction = new LStore(2)
					break
				case 0x42:
					instruction = new LStore(3)
					break
				case 0x43:
					instruction = new FStore(0)
					break
				case 0x44:
					instruction = new FStore(1)
					break
				case 0x45:
					instruction = new FStore(2)
					break
				case 0x46:
					instruction = new FStore(3)
					break
				case 0x47:
					instruction = new DStore(0)
					break
				case 0x48:
					instruction = new DStore(1)
					break
				case 0x49:
					instruction = new DStore(2)
					break
				case 0x4a:
					instruction = new DStore(3)
					break
				case 0x4b:
					instruction = new AStore(0)
					break
				case 0x4c:
					instruction = new AStore(1)
					break
				case 0x4d:
					instruction = new AStore(2)
					break
				case 0x4e:
					instruction = new AStore(3)
					break
				case 0x4f:
					instruction = new IAStore
					break
				case 0x50:
					instruction = new LAStore
					break
				case 0x51:
					instruction = new FAStore
					break
				case 0x52:
					instruction = new DAStore
					break
				case 0x53:
					instruction = new AAStore
					break
				case 0x54:
					instruction = new BAStore
					break
				case 0x55:
					instruction = new CAStore
					break
				case 0x56:
					instruction = new SAStore
					break
				case 0x57:
					instruction = new Pop(1)
					break
				case 0x58:
					instruction = new Pop(2)
					break
				case 0x59:
					instruction = new Dup
					break
				case 0x60:
					instruction = new IAdd
					break
				case 0x61:
					instruction = new LAdd
					break
				case 0x62:
					instruction = new FAdd
					break
				case 0x63:
					instruction = new DAdd
					break
				case 0x64:
					instruction = new ISub
					break
				case 0x65:
					instruction = new LSub
					break
				case 0x66:
					instruction = new FSub
					break
				case 0x67:
					instruction = new DSub
					break
				case 0x68:
					instruction = new IMul
					break
				case 0x69:
					instruction = new LMul
					break
				case 0x6a:
					instruction = new FMul
					break
				case 0x6b:
					instruction = new DMul
					break
				case 0x6c:
					instruction = new IDiv
					break
				case 0x6d:
					instruction = new LDiv
					break
				case 0x6e:
					instruction = new FDiv
					break
				case 0x6f:
					instruction = new DDiv
					break
				case 0x70:
					instruction = new IRem
					break
				case 0x71:
					instruction = new LRem
					break
				case 0x72:
					instruction = new FRem
					break
				case 0x73:
					instruction = new DRem
					break
				case 0x74:
					instruction = new INeg
					break
				case 0x75:
					instruction = new LNeg
					break
				case 0x76:
					instruction = new FNeg
					break
				case 0x77:
					instruction = new DNeg
					break
				case 0x78:
					instruction = new IShiftLeft
					break
				case 0x79:
					instruction = new LShiftLeft
					break
				case 0x7a:
					instruction = new IShiftRight
					break
				case 0x7b:
					instruction = new LShiftRight
					break
				case 0x7c:
					instruction = new IUShiftRight
					break
				case 0x7d:
					instruction = new LUShiftRight
					break
				case 0x7e:
					instruction = new IAnd
					break
				case 0x7f:
					instruction = new LAnd
					break
				case 0x80:
					instruction = new IOr
					break
				case 0x81:
					instruction = new LOr
					break
				case 0x82:
					instruction = new IXor
					break
				case 0x83:
					instruction = new LXor
					break
				case 0x84: {
					const {index, newOffset} = (wide ? doubleByteIndex : singleByteIndex)(data, offset)
					offset = newOffset
					const {result: value, length} = (wide ? parseSignedShort : parseSignedByte)(data, offset)
					instruction = new IInc(index, value)
					offset += length
					break
				}
				case 0x85:
					instruction = new I2L
					break
				case 0x86:
					instruction = new I2F
					break
				case 0x87:
					instruction = new I2D
					break
				case 0x88:
					instruction = new L2I
					break
				case 0x89:
					instruction = new L2F
					break
				case 0x8a:
					instruction = new L2D
					break
				case 0x8b:
					instruction = new F2I
					break
				case 0x8c:
					instruction = new F2L
					break
				case 0x8d:
					instruction = new F2D
					break
				case 0x8e:
					instruction = new D2I
					break
				case 0x8f:
					instruction = new D2L
					break
				case 0x90:
					instruction = new D2F
					break
				case 0x91:
					instruction = new I2B
					break
				case 0x92:
					instruction = new I2C
					break
				case 0x93:
					instruction = new I2S
					break
				case 0x94:
					instruction = new LCmp
					break
				case 0x95:
					instruction = new FCmpL
					break
				case 0x96:
					instruction = new FCmpG
					break
				case 0x97:
					instruction = new DCmpL
					break
				case 0x98:
					instruction = new DCmpG
					break
				case 0x99:
					({instruction, newOffset} = jumpOperation(data, offset, IfEq))
					break
				case 0x9a:
					({instruction, newOffset} = jumpOperation(data, offset, IfNe))
					break
				case 0x9b:
					({instruction, newOffset} = jumpOperation(data, offset, IfLt))
					break
				case 0x9c:
					({instruction, newOffset} = jumpOperation(data, offset, IfGe))
					break
				case 0x9d:
					({instruction, newOffset} = jumpOperation(data, offset, IfGt))
					break
				case 0x9e:
					({instruction, newOffset} = jumpOperation(data, offset, IfLe))
					break
				case 0x9f:
					({instruction, newOffset} = jumpOperation(data, offset, IfICmpEq))
					break
				case 0xa0:
					({instruction, newOffset} = jumpOperation(data, offset, IfICmpNe))
					break
				case 0xa1:
					({instruction, newOffset} = jumpOperation(data, offset, IfICmpLt))
					break
				case 0xa2:
					({instruction, newOffset} = jumpOperation(data, offset, IfICmpGe))
					break
				case 0xa3:
					({instruction, newOffset} = jumpOperation(data, offset, IfICmpGt))
					break
				case 0xa4:
					({instruction, newOffset} = jumpOperation(data, offset, IfICmpLe))
					break
				case 0xa5:
					({instruction, newOffset} = jumpOperation(data, offset, IfACmpEq))
					break
				case 0xa6:
					({instruction, newOffset} = jumpOperation(data, offset, IfACmpNe))
					break
				case 0xa7:
					({instruction, newOffset} = jumpOperation(data, offset, Goto))
					break
				case 0xaa: {
					offset += 3 - ((offset + 3) % 4) //0-3 bytes of padding
					const {result, length} = parseTableSwitch(slice(data, offset))
					instruction = result
					offset += length
					break
				}
				case 0xac:
					instruction = new IReturn
					break
				case 0xad:
					instruction = new LReturn
					break
				case 0xae:
					instruction = new FReturn
					break
				case 0xaf:
					instruction = new DReturn
					break
				case 0xb0:
					instruction = new AReturn
					break
				case 0xb1:
					instruction = new Return
					break
				case 0xb2: {
					const {result: field, length} = constantParser(slice(data, offset))
					instruction = new GetStatic(field.getValue(constantPool))
					offset += length
					break
				}
				case 0xb6: {
					const {result: method, length} = constantParser(slice(data, offset))
					instruction = new InvokeVirtual(method.getValue(constantPool))
					offset += length
					break
				}
				case 0xb7: {
					const {result: method, length} = constantParser(slice(data, offset))
					instruction = new InvokeSpecial(method.getValue(constantPool))
					offset += length
					break
				}
				case 0xb8: {
					const {result: method, length} = constantParser(slice(data, offset))
					instruction = new InvokeStatic(method.getValue(constantPool))
					offset += length
					break
				}
				case 0xb9: {
					const {result: method, length} = constantParser(slice(data, offset))
					instruction = new InvokeInterface(method.getValue(constantPool))
					offset += length + 2 //skip 2 count bytes
					break
				}
				case 0xbb: {
					const {result: clazz, length} = constantParser(slice(data, offset))
					instruction = new New(clazz.getValue(constantPool))
					offset += length
					break
				}
				case 0xbc: {
					const type = data.getUint8(offset++)
					let primitive: Primitive
					switch (type) {
						case 4:
							primitive = 'boolean'
							break
						case 5:
							primitive = 'char'
							break
						case 6:
							primitive = 'float'
							break
						case 7:
							primitive = 'double'
							break
						case 8:
							primitive = 'byte'
							break
						case 9:
							primitive = 'short'
							break
						case 10:
							primitive = 'int'
							break
						case 11:
							primitive = 'long'
							break
						default:
							throw new Error('Unrecognized primitive type: ' + String(type))
					}
					instruction = new NewPrimitiveArray(primitive)
					break
				}
				case 0xbd: {
					const {result: clazz, length} = constantParser(slice(data, offset))
					instruction = new ANewArray(clazz.getValue(constantPool))
					offset += length
					break
				}
				case 0xbe:
					instruction = new ArrayLength
					break
				case 0xbf:
					instruction = new AThrow
					break
				case 0xc0: {
					const {result: clazz, length} = constantParser(slice(data, offset))
					instruction = new CheckCast(clazz.getValue(constantPool))
					offset += length
					break
				}
				default: throw new Error('Unknown opcode: 0x' + opCode.toString(16))
			}
			if (newOffset !== undefined) offset = newOffset
			instructions.set(opCodeOffset, {
				instruction,
				length: offset - opCodeOffset
			})
		}
		return parseReturn(instructions)
	})
process.on('exit', () => {
	console.log(
		[...bytecodesFound]
			.sort((a, b) => a - b)
			.map(x => '0x' + x.toString(16))
	)
	let i: number
	for (i = 0xaa; bytecodesFound.has(i); i++); //skip dup/swap and jsr/ret instructions
	console.log('0x' + i.toString(16))
})