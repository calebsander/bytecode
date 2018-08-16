import {
	parseInt,
	parseByteArray,
	parseAndThen,
	parseReturn,
	parseStruct,
	parseTimes,
	slice
} from './parse'
import {constantParser} from './constant-parser'
import {ConstantPool, Class, LiteralConstant, Ref} from './constant-pool-parser'
import * as i from './instructions'
import {Primitive} from './variable-types'

const LDC = 0x12
const GOTO = 0xa7

interface InstructionLength {
	instruction: i.Instruction
	length: number
}
export type Code = Map<number, InstructionLength>

interface LoadStoreConstructor {
	new(index: number, isStatic: boolean): i.LoadStoreInstruction
}
function singleByteIndex(data: DataView, offset: number) {
	const index = data.getUint8(offset)
	return {index, newOffset: offset + 1}
}
function doubleByteIndex(data: DataView, offset: number) {
	const index = data.getUint16(offset)
	return {index, newOffset: offset + 2}
}
function localInstruction(data: DataView, offset: number, clazz: LoadStoreConstructor, isStatic?: boolean) {
	const {index, newOffset} = singleByteIndex(data, offset)
	return {instruction: new clazz(index, isStatic!), newOffset}
}
interface JumpConstructor {
	new(offset: number): i.Jump
}
function jumpInstruction(data: DataView, offset: number, clazz: JumpConstructor, wide = false) {
	const {result: jumpOffset, length} = wide
		? parseSignedInt(slice(data, offset))
		: parseSignedShort(data, offset)
	return {instruction: new clazz(jumpOffset), newOffset: offset + length}
}
type Constant = Ref | Class | LiteralConstant
interface ConstantConstructor<E extends Constant> {
	new(constant: E): i.Instruction
}
function constantInstruction<E extends Constant>(
	data: DataView,
	offset: number,
	clazz: ConstantConstructor<E>,
	constantPool: ConstantPool
) {
	const {result, length} = constantParser(slice(data, offset))
	return {
		instruction: new clazz(result.getValue(constantPool)),
		newOffset: offset + length
	}
}
const parseSignedByte = (data: DataView, offset: number) =>
	({result: data.getInt8(offset), length: 1})
const parseSignedShort = (data: DataView, offset: number) =>
	({result: data.getInt16(offset), length: 2})
const parseSignedInt = parseAndThen(parseInt, int => parseReturn(int | 0))
const parseTableSwitch =
	parseAndThen(parseSignedInt, defaultOffset =>
		parseAndThen(parseSignedInt, low =>
			parseAndThen(parseSignedInt, high =>
				parseAndThen(
					parseTimes(parseSignedInt, high - low + 1),
					jumpOffsets => {
						const offsetMap = new Map<number, number>()
						for (let i = low; i <= high; i++) offsetMap.set(i, jumpOffsets[i - low])
						return parseReturn(new i.SwitchInstruction(offsetMap, defaultOffset))
					}
				)
			)
		)
	)
interface MatchOffsetPair {
	match: number
	offset: number
}
const parseMatchOffset = parseStruct<MatchOffsetPair>([
	['match', parseSignedInt],
	['offset', parseSignedInt]
])
const parseLookupSwitch =
	parseAndThen(parseSignedInt, defaultOffset =>
		parseAndThen(parseSignedInt, nPairs => {
			if (nPairs < 0) throw new Error('nPairs must be nonnegative')
			return parseAndThen(
				parseTimes(parseMatchOffset, nPairs),
				matchOffsets => {
					const offsetMap = new Map<number, number>()
					for (const {match, offset} of matchOffsets) offsetMap.set(match, offset)
					return parseReturn(new i.SwitchInstruction(offsetMap, defaultOffset))
				}
			)
		})
	)
const pad = (offset: number) =>
	offset + 3 - ((offset + 3) & 3) //0-3 bytes of padding

export interface MethodContext {
	constantPool: ConstantPool
	className: string
	isStatic: boolean
}

const parseCode = parseByteArray(parseInt)
export const bytecodeParser = ({constantPool, className, isStatic}: MethodContext) =>
	parseAndThen(parseCode, bytecode => {
		const data = new DataView(bytecode)
		const instructions: Code = new Map
		let offset = 0
		while (offset < bytecode.byteLength) {
			const opCodeOffset = offset
			let opCode = data.getUint8(offset++)
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
			else if (opCode === 0xc8) { //wide goto
				wide = true
				opCode = GOTO
			}
			else wide = false
			let instruction: i.Instruction
			let newOffset: number | undefined
			switch (opCode) {
				case 0x00:
					instruction = new i.NoOp
					break
				case 0x01:
					instruction = new i.NullConst
					break
				case 0x02:
					instruction = new i.IConst(-1)
					break
				case 0x03:
					instruction = new i.IConst(0)
					break
				case 0x04:
					instruction = new i.IConst(1)
					break
				case 0x05:
					instruction = new i.IConst(2)
					break
				case 0x06:
					instruction = new i.IConst(3)
					break
				case 0x07:
					instruction = new i.IConst(4)
					break
				case 0x08:
					instruction = new i.IConst(5)
					break
				case 0x09:
					instruction = new i.LConst(0n)
					break
				case 0x0a:
					instruction = new i.LConst(1n)
					break
				case 0x0b:
					instruction = new i.FConst(0)
					break
				case 0x0c:
					instruction = new i.FConst(1)
					break
				case 0x0d:
					instruction = new i.FConst(2)
					break
				case 0x0e:
					instruction = new i.DConst(0)
					break
				case 0x0f:
					instruction = new i.DConst(1)
					break
				case 0x10:
					instruction = new i.IConst(data.getInt8(offset++))
					break
				case 0x11: {
					const {result, length} = parseSignedShort(data, offset)
					instruction = new i.SPush(result)
					offset += length
					break
				}
				case LDC: {
					let index: number
					({index, newOffset} = (wide ? doubleByteIndex : singleByteIndex)(data, offset))
					instruction = new i.LoadConstant(constantPool.getConstant(index) as LiteralConstant)
					break
				}
				//case 0x13 (ldc_w) handled above
				case 0x14:
					({instruction, newOffset} = constantInstruction(data, offset, i.LoadConstant, constantPool))
					break
				case 0x15:
					({instruction, newOffset} = localInstruction(data, offset, i.ILoad))
					break
				case 0x16:
					({instruction, newOffset} = localInstruction(data, offset, i.LLoad))
					break
				case 0x17:
					({instruction, newOffset} = localInstruction(data, offset, i.FLoad))
					break
				case 0x18:
					({instruction, newOffset} = localInstruction(data, offset, i.DLoad))
					break
				case 0x19:
					({instruction, newOffset} = localInstruction(data, offset, i.ALoad, isStatic))
					break
				case 0x1a:
					instruction = new i.ILoad(0)
					break
				case 0x1b:
					instruction = new i.ILoad(1)
					break
				case 0x1c:
					instruction = new i.ILoad(2)
					break
				case 0x1d:
					instruction = new i.ILoad(3)
					break
				case 0x1e:
					instruction = new i.LLoad(0)
					break
				case 0x1f:
					instruction = new i.LLoad(1)
					break
				case 0x20:
					instruction = new i.LLoad(2)
					break
				case 0x21:
					instruction = new i.LLoad(3)
					break
				case 0x22:
					instruction = new i.FLoad(0)
					break
				case 0x23:
					instruction = new i.FLoad(1)
					break
				case 0x24:
					instruction = new i.FLoad(2)
					break
				case 0x25:
					instruction = new i.FLoad(3)
					break
				case 0x26:
					instruction = new i.DLoad(0)
					break
				case 0x27:
					instruction = new i.DLoad(1)
					break
				case 0x28:
					instruction = new i.DLoad(2)
					break
				case 0x29:
					instruction = new i.DLoad(3)
					break
				case 0x2a:
					instruction = new i.ALoad(0, isStatic)
					break
				case 0x2b:
					instruction = new i.ALoad(1, isStatic)
					break
				case 0x2c:
					instruction = new i.ALoad(2, isStatic)
					break
				case 0x2d:
					instruction = new i.ALoad(3, isStatic)
					break
				case 0x2e:
					instruction = new i.IALoad
					break
				case 0x2f:
					instruction = new i.LALoad
					break
				case 0x30:
					instruction = new i.FALoad
					break
				case 0x31:
					instruction = new i.DALoad
					break
				case 0x32:
					instruction = new i.AALoad
					break
				case 0x33:
					instruction = new i.BALoad
					break
				case 0x34:
					instruction = new i.CALoad
					break
				case 0x35:
					instruction = new i.SALoad
					break
				case 0x36:
					({instruction, newOffset} = localInstruction(data, offset, i.IStore))
					break
				case 0x37:
					({instruction, newOffset} = localInstruction(data, offset, i.LStore))
					break
				case 0x38:
					({instruction, newOffset} = localInstruction(data, offset, i.FStore))
					break
				case 0x39:
					({instruction, newOffset} = localInstruction(data, offset, i.DStore))
					break
				case 0x3a:
					({instruction, newOffset} = localInstruction(data, offset, i.AStore))
					break
				case 0x3b:
					instruction = new i.IStore(0)
					break
				case 0x3c:
					instruction = new i.IStore(1)
					break
				case 0x3d:
					instruction = new i.IStore(2)
					break
				case 0x3e:
					instruction = new i.IStore(3)
					break
				case 0x3f:
					instruction = new i.LStore(0)
					break
				case 0x40:
					instruction = new i.LStore(1)
					break
				case 0x41:
					instruction = new i.LStore(2)
					break
				case 0x42:
					instruction = new i.LStore(3)
					break
				case 0x43:
					instruction = new i.FStore(0)
					break
				case 0x44:
					instruction = new i.FStore(1)
					break
				case 0x45:
					instruction = new i.FStore(2)
					break
				case 0x46:
					instruction = new i.FStore(3)
					break
				case 0x47:
					instruction = new i.DStore(0)
					break
				case 0x48:
					instruction = new i.DStore(1)
					break
				case 0x49:
					instruction = new i.DStore(2)
					break
				case 0x4a:
					instruction = new i.DStore(3)
					break
				case 0x4b:
					instruction = new i.AStore(0)
					break
				case 0x4c:
					instruction = new i.AStore(1)
					break
				case 0x4d:
					instruction = new i.AStore(2)
					break
				case 0x4e:
					instruction = new i.AStore(3)
					break
				case 0x4f:
					instruction = new i.IAStore
					break
				case 0x50:
					instruction = new i.LAStore
					break
				case 0x51:
					instruction = new i.FAStore
					break
				case 0x52:
					instruction = new i.DAStore
					break
				case 0x53:
					instruction = new i.AAStore
					break
				case 0x54:
					instruction = new i.BAStore
					break
				case 0x55:
					instruction = new i.CAStore
					break
				case 0x56:
					instruction = new i.SAStore
					break
				case 0x57:
					instruction = new i.Pop(1)
					break
				case 0x58:
					instruction = new i.Pop(2)
					break
				case 0x59:
					instruction = new i.Dup
					break
				case 0x60:
					instruction = new i.IAdd
					break
				case 0x61:
					instruction = new i.LAdd
					break
				case 0x62:
					instruction = new i.FAdd
					break
				case 0x63:
					instruction = new i.DAdd
					break
				case 0x64:
					instruction = new i.ISub
					break
				case 0x65:
					instruction = new i.LSub
					break
				case 0x66:
					instruction = new i.FSub
					break
				case 0x67:
					instruction = new i.DSub
					break
				case 0x68:
					instruction = new i.IMul
					break
				case 0x69:
					instruction = new i.LMul
					break
				case 0x6a:
					instruction = new i.FMul
					break
				case 0x6b:
					instruction = new i.DMul
					break
				case 0x6c:
					instruction = new i.IDiv
					break
				case 0x6d:
					instruction = new i.LDiv
					break
				case 0x6e:
					instruction = new i.FDiv
					break
				case 0x6f:
					instruction = new i.DDiv
					break
				case 0x70:
					instruction = new i.IRem
					break
				case 0x71:
					instruction = new i.LRem
					break
				case 0x72:
					instruction = new i.FRem
					break
				case 0x73:
					instruction = new i.DRem
					break
				case 0x74:
					instruction = new i.INeg
					break
				case 0x75:
					instruction = new i.LNeg
					break
				case 0x76:
					instruction = new i.FNeg
					break
				case 0x77:
					instruction = new i.DNeg
					break
				case 0x78:
					instruction = new i.IShiftLeft
					break
				case 0x79:
					instruction = new i.LShiftLeft
					break
				case 0x7a:
					instruction = new i.IShiftRight
					break
				case 0x7b:
					instruction = new i.LShiftRight
					break
				case 0x7c:
					instruction = new i.IUShiftRight
					break
				case 0x7d:
					instruction = new i.LUShiftRight
					break
				case 0x7e:
					instruction = new i.IAnd
					break
				case 0x7f:
					instruction = new i.LAnd
					break
				case 0x80:
					instruction = new i.IOr
					break
				case 0x81:
					instruction = new i.LOr
					break
				case 0x82:
					instruction = new i.IXor
					break
				case 0x83:
					instruction = new i.LXor
					break
				case 0x84: {
					let index: number
					({index, newOffset} = (wide ? doubleByteIndex : singleByteIndex)(data, offset))
					const {result, length} = (wide ? parseSignedShort : parseSignedByte)(data, newOffset)
					instruction = new i.IInc(index, result)
					newOffset += length
					break
				}
				case 0x85:
					instruction = new i.I2L
					break
				case 0x86:
					instruction = new i.I2F
					break
				case 0x87:
					instruction = new i.I2D
					break
				case 0x88:
					instruction = new i.L2I
					break
				case 0x89:
					instruction = new i.L2F
					break
				case 0x8a:
					instruction = new i.L2D
					break
				case 0x8b:
					instruction = new i.F2I
					break
				case 0x8c:
					instruction = new i.F2L
					break
				case 0x8d:
					instruction = new i.F2D
					break
				case 0x8e:
					instruction = new i.D2I
					break
				case 0x8f:
					instruction = new i.D2L
					break
				case 0x90:
					instruction = new i.D2F
					break
				case 0x91:
					instruction = new i.I2B
					break
				case 0x92:
					instruction = new i.I2C
					break
				case 0x93:
					instruction = new i.I2S
					break
				case 0x94:
					instruction = new i.LCmp
					break
				case 0x95:
					instruction = new i.FCmpL
					break
				case 0x96:
					instruction = new i.FCmpG
					break
				case 0x97:
					instruction = new i.DCmpL
					break
				case 0x98:
					instruction = new i.DCmpG
					break
				case 0x99:
					({instruction, newOffset} = jumpInstruction(data, offset, i.IfEq))
					break
				case 0x9a:
					({instruction, newOffset} = jumpInstruction(data, offset, i.IfNe))
					break
				case 0x9b:
					({instruction, newOffset} = jumpInstruction(data, offset, i.IfLt))
					break
				case 0x9c:
					({instruction, newOffset} = jumpInstruction(data, offset, i.IfGe))
					break
				case 0x9d:
					({instruction, newOffset} = jumpInstruction(data, offset, i.IfGt))
					break
				case 0x9e:
					({instruction, newOffset} = jumpInstruction(data, offset, i.IfLe))
					break
				case 0x9f:
					({instruction, newOffset} = jumpInstruction(data, offset, i.IfICmpEq))
					break
				case 0xa0:
					({instruction, newOffset} = jumpInstruction(data, offset, i.IfICmpNe))
					break
				case 0xa1:
					({instruction, newOffset} = jumpInstruction(data, offset, i.IfICmpLt))
					break
				case 0xa2:
					({instruction, newOffset} = jumpInstruction(data, offset, i.IfICmpGe))
					break
				case 0xa3:
					({instruction, newOffset} = jumpInstruction(data, offset, i.IfICmpGt))
					break
				case 0xa4:
					({instruction, newOffset} = jumpInstruction(data, offset, i.IfICmpLe))
					break
				case 0xa5:
					({instruction, newOffset} = jumpInstruction(data, offset, i.IfACmpEq))
					break
				case 0xa6:
					({instruction, newOffset} = jumpInstruction(data, offset, i.IfACmpNe))
					break
				case GOTO:
					({instruction, newOffset} = jumpInstruction(data, offset, i.Goto, wide))
					break
				case 0xaa: {
					offset = pad(offset)
					const {result, length} = parseTableSwitch(slice(data, offset))
					instruction = result
					offset += length
					break
				}
				case 0xab: {
					offset = pad(offset)
					const {result, length} = parseLookupSwitch(slice(data, offset))
					instruction = result
					offset += length
					break
				}
				case 0xac:
					instruction = new i.IReturn
					break
				case 0xad:
					instruction = new i.LReturn
					break
				case 0xae:
					instruction = new i.FReturn
					break
				case 0xaf:
					instruction = new i.DReturn
					break
				case 0xb0:
					instruction = new i.AReturn
					break
				case 0xb1:
					instruction = new i.Return
					break
				case 0xb2:
					({instruction, newOffset} = constantInstruction(data, offset, i.GetStatic, constantPool))
					break
				case 0xb3:
					({instruction, newOffset} = constantInstruction(data, offset, i.PutStatic, constantPool))
					break
				case 0xb4:
					({instruction, newOffset} = constantInstruction(data, offset, i.GetField, constantPool))
					break
				case 0xb5:
					({instruction, newOffset} = constantInstruction(data, offset, i.PutField, constantPool))
					break
				case 0xb6: {
					const {result, length} = constantParser(slice(data, offset))
					instruction = new i.InvokeVirtual(className, result.getValue(constantPool))
					offset += length
					break
				}
				case 0xb7: {
					const {result, length} = constantParser(slice(data, offset))
					instruction = new i.InvokeSpecial(className, result.getValue(constantPool))
					offset += length
					break
				}
				case 0xb8: {
					const {result, length} = constantParser(slice(data, offset))
					instruction = new i.InvokeStatic(className, result.getValue(constantPool))
					offset += length
					break
				}
				case 0xb9: {
					const {result, length} = constantParser(slice(data, offset))
					instruction = new i.InvokeInterface(className, result.getValue(constantPool))
					offset += length + 2 //skip 2 count bytes
					break
				}
				case 0xbb: {
					const {result, length} = constantParser(slice(data, offset))
					instruction = new i.New(result.getValue(constantPool))
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
							throw new Error(`Unrecognized primitive type: ${type}`)
					}
					instruction = new i.NewPrimitiveArray(primitive)
					break
				}
				case 0xbd:
					({instruction, newOffset} = constantInstruction(data, offset, i.ANewArray, constantPool))
					break
				case 0xbe:
					instruction = new i.ArrayLength
					break
				case 0xbf:
					instruction = new i.AThrow
					break
				case 0xc0:
					({instruction, newOffset} = constantInstruction(data, offset, i.CheckCast, constantPool))
					break
				case 0xc1:
					({instruction, newOffset} = constantInstruction(data, offset, i.InstanceOf, constantPool))
					break
				case 0xc2:
					instruction = new i.MonitorEnter
					break
				case 0xc3:
					instruction = new i.MonitorExit
					break
				//case 0xc4 (wide) handled above
				case 0xc5: {
					const {result: clazz, length} = constantParser(slice(data, offset))
					offset += length
					const dimensions = data.getUint8(offset++)
					instruction = new i.MultiANewArray(clazz.getValue(constantPool), dimensions)
					break
				}
				case 0xc6:
					({instruction, newOffset} = jumpInstruction(data, offset, i.IfNull))
					break
				case 0xc7:
					({instruction, newOffset} = jumpInstruction(data, offset, i.IfNonNull))
					break
				//case 0xc8 (goto_w) handled above
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