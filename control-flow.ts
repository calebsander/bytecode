import {
	BinaryOperation,
	Block,
	BooleanLiteral,
	BreakStatement,
	Case,
	Catch,
	ContinueStatement,
	Expression,
	IfStatement,
	IntegerLiteral,
	LoopReference,
	Ternary,
	UnaryOperation,
	WhileStatement,
	SwitchStatement,
	SynchronizedStatement,
	TryStatement,
	Variable,
	ClassReference
} from './ast'
import {Code} from './bytecode-parser'
import {Class} from './constant-pool-parser'
import {ExceptionTable, ExceptionTableEntry} from './exception-table-parser'
import {
	forcePop,
	AStore,
	Goto,
	Jump,
	MonitorEnter,
	MonitorExit,
	Stack,
	SwitchInstruction
} from './instructions'

interface LoopCounter {
	count: number
}
type Loops = Map<number, LoopReference>
interface CatchType {
	pc: number //first instruction of the catch block
	clazz: Class | null //exception catch to match, or null for any
}

class IfExceedsBoundsError extends Error {
	constructor(
		public readonly instructionAfterIf: number,
		public readonly target: number
	) { super('If statement exceeds block') }
}
class MonitorExitError extends Error {
	constructor(public readonly nextInstruction: number) {
		super('Cannot execute monitorexit')
	}
}

const getCaseExpression = (value: number | null) =>
	value === null ? null : new IntegerLiteral(value)

function parseControlFlow(
	instructions: Code,
	exceptionTable: ExceptionTable,
	start: number, //inclusive
	end: number, //exclusive
	loopCounter: LoopCounter,
	breaks: Loops,
	continues: Loops,
	stack: Stack,
	block: Block = []
): Block {
	function getBreak(index: number) {
		const {instruction} = instructions.get(index)!
		return instruction instanceof Jump
			? breaks.get(index + instruction.offset)
			: undefined
	}
	function getContinue(index: number) {
		const {instruction} = instructions.get(index)!
		return instruction instanceof Jump
			? continues.get(index + instruction.offset)
			: undefined
	}
	const preceding = new Map<number, number>()
	let firstJumpForward = Infinity, //index of the instruction after the if jump
	    firstJumpForwardTarget: number, //index of the start of the else block, or after the if statement
	    firstJumpBack = Infinity, //index of the target of the do-while jump back
	    firstJumpBackSource: number, //index of the do-while jump
	    firstSwitch = Infinity, //index of tableswitch instruction
	    firstMonitor = Infinity //index of monitorenter instruction
	for (let index = start; index < end;) {
		const instructionLength = instructions.get(index)
		if (!instructionLength) throw new Error(`Missing instruction at ${index}`)
		const {instruction, length} = instructionLength
		const nextIndex = index + length
		preceding.set(nextIndex, index)
		if (instruction instanceof SwitchInstruction) firstSwitch = Math.min(firstSwitch, index)
		else if (instruction instanceof MonitorEnter) firstMonitor = Math.min(firstMonitor, index)
		// Breaks and continues don't need to be processed as separate blocks
		else if (instruction instanceof Jump && !(getBreak(index) || getContinue(index))) {
			const {offset} = instruction
			const target = index + offset
			if (offset < 0) {
				if (target < firstJumpBack) {
					firstJumpBack = target
					firstJumpBackSource = index
				}
				else if (target === firstJumpBack) {
					firstJumpBackSource = Math.max(firstJumpBackSource!, index)
				}
			}
			else if (offset > 0) {
				if (nextIndex < firstJumpForward) {
					firstJumpForward = nextIndex
					firstJumpForwardTarget = target
				}
				else if (nextIndex === firstJumpForward) {
					firstJumpForwardTarget = Math.max(firstJumpForwardTarget!, target)
				}
			}
			else throw new Error('Jump with offset 0?')
		}
		index = nextIndex
	}
	let firstTryStart = Infinity, //index of the first instruction of the try block
	    firstTryEnd: number, //index of the instruction after the try block
	    firstTryCatches: CatchType[]
	for (const {startPC, endPC, handlerPC, catchType} of exceptionTable) {
		if (startPC < start || endPC > end) continue
		let earlierCatch = false
		let addCatchType = false
		if (startPC < firstTryStart) {
			firstTryStart = startPC
			earlierCatch = true
		}
		else if (startPC === firstTryStart) {
			if (endPC > firstTryEnd!) earlierCatch = true
			else if (endPC === firstTryEnd) addCatchType = true
		}
		if (earlierCatch) {
			firstTryEnd = endPC
			firstTryCatches = []
			addCatchType = true
		}
		if (addCatchType) firstTryCatches!.push({pc: handlerPC, clazz: catchType})
	}
	const firstControl = Math.min(
		firstJumpForward,
		firstJumpBack,
		firstSwitch,
		firstMonitor,
		firstTryStart,
		end
	)
	for (let index = start; index < firstControl;) {
		const instructionLength = instructions.get(index)
		if (!instructionLength) throw new Error(`Missing instruction at ${index}`)
		const {instruction, length} = instructionLength
		if (instruction instanceof MonitorExit) throw new MonitorExitError(index + length)
		instruction.execute(stack, block)
		const breakLoop = getBreak(index)
		if (breakLoop) {
			block.push(new IfStatement(
				forcePop(stack),
				[new BreakStatement(breakLoop)]
			))
		}
		else {
			const continueLoop = getContinue(index)
			if (continueLoop) {
				block.push(new IfStatement(
					forcePop(stack),
					[new ContinueStatement(continueLoop)]
				))
			}
		}
		index += length
	}
	let controlResume: number | undefined
	if (firstControl === firstJumpForward) {
		if (firstJumpForwardTarget! > end) throw new IfExceedsBoundsError(firstJumpForward, firstJumpForwardTarget)
		const elseCondition = forcePop(stack)
		const lastIndexOfIf = preceding.get(firstJumpForwardTarget)!
		const lastInstructionOfIf = instructions.get(lastIndexOfIf)!.instruction
		let hasElseBlock = false
		let ifBlockEnd: number
		let elseBlock: Block = [], elseStack: Stack = []
		if (lastInstructionOfIf instanceof Goto) {
			const elseBlockEnd = lastIndexOfIf + lastInstructionOfIf.offset
			if (elseBlockEnd >= firstJumpForwardTarget && !(getBreak(lastIndexOfIf) || getContinue(lastIndexOfIf))) {
				hasElseBlock = true
				controlResume = elseBlockEnd
				ifBlockEnd = lastIndexOfIf
				parseControlFlow(
					instructions, exceptionTable,
					firstJumpForwardTarget, elseBlockEnd,
					loopCounter, breaks, continues,
					elseStack, elseBlock
				)
			}
		}
		if (!hasElseBlock) controlResume = ifBlockEnd = firstJumpForwardTarget
		if (!elseBlock.length && elseStack.length === 1) { //&& or ternary
			const elseConditions = [elseCondition] //contains expressions causing jumps to else block
			let ifResult: Expression | undefined
			let clauseStart = firstJumpForward
			do {
				const clauseStack: Stack = []
				const clauseBlock: Block = []
				try {
					parseControlFlow(
						instructions, exceptionTable,
						clauseStart, ifBlockEnd!,
						loopCounter, breaks, continues,
						clauseStack, clauseBlock
					)
					;[ifResult] = clauseStack
				} catch (e) {
					if (e instanceof IfExceedsBoundsError && e.target === firstJumpForwardTarget) {
						elseConditions.push(clauseStack[0])
						clauseStart = e.instructionAfterIf
					}
					else throw e
				}
				if (clauseBlock.length || clauseStack.length !== 1) throw new Error('Invalid &&/ternary block')
			} while (!ifResult)
			stack.push(new Ternary(
				elseConditions
					.map<Expression>(exp => new UnaryOperation('!', exp))
					.reduceRight((rightCond, leftCond) =>
						new BinaryOperation('&&', leftCond, rightCond)
					),
				ifResult,
				elseStack[0]
			))
		}
		else { //|| or if block
			if (elseStack.length) throw new Error('Expected empty stack')
			let isOr = false
			if (!hasElseBlock) {
				const elseJumpIndex = preceding.get(ifBlockEnd!)!
				const elseJump = instructions.get(elseJumpIndex)!.instruction
				if (elseJump instanceof Jump) {
					const elseTarget = elseJumpIndex + elseJump.offset
					if (elseTarget > ifBlockEnd!&& !(breaks.has(elseTarget) || continues.has(elseTarget))) {
						isOr = true
						const gotoEndIndex = preceding.get(elseTarget)!
						const gotoEnd = instructions.get(gotoEndIndex)!.instruction
						if (!(gotoEnd instanceof Goto)) throw new Error('Expected Goto in || ternary')
						controlResume = gotoEndIndex + gotoEnd.offset
						//Recurse - if we are parsing, e.g. A || B ? 1 : 0, this will result in B ? 1 : 0
						const ternaryStack: Stack = []
						const ternaryBlock = parseControlFlow(
							instructions, exceptionTable,
							firstJumpForward, controlResume,
							loopCounter, breaks, continues,
							ternaryStack
						)
						const [ternary] = ternaryStack
						if (!(!ternaryBlock.length && ternaryStack.length === 1 && ternary instanceof Ternary)) {
							throw new Error('Invalid || block')
						}
						const {cond, ifTrue, ifFalse} = ternary
						stack.push(new Ternary(
							new BinaryOperation('||', elseCondition, cond),
							ifTrue,
							ifFalse
						))
					}
				}
			}
			if (!isOr) {
				const ifStack: Stack = []
				const ifBlock = parseControlFlow(
					instructions, exceptionTable,
					firstJumpForward, ifBlockEnd!,
					loopCounter, breaks, continues,
					ifStack
				)
				if (ifStack.length) throw new Error('Expected empty stack')
				block.push(new IfStatement(new UnaryOperation('!', elseCondition), ifBlock, elseBlock))
			}
		}
	}
	else if (firstControl === firstJumpBack) {
		if (firstJumpBackSource! > end) throw new Error('While loop exceeds block')
		controlResume = firstJumpBackSource + instructions.get(firstJumpBackSource)!.length
		const loop = {label: `loop${loopCounter.count++}`}
		breaks.set(controlResume, loop)
		continues.set(firstJumpBack, loop)
		const whileStack: Stack = []
		const whileBlock = parseControlFlow(
			instructions, exceptionTable,
			firstJumpBack, controlResume,
			loopCounter, breaks, continues,
			whileStack
		)
		if (whileStack.length) throw new Error('Expected empty while stack')
		breaks.delete(controlResume)
		continues.delete(firstJumpBack)
		block.push(new WhileStatement(
			new BooleanLiteral(false),
			whileBlock,
			true,
			loop
		))
	}
	else if (firstControl === firstSwitch) {
		const {offsetMap, defaultOffset} = instructions.get(firstSwitch)!.instruction as SwitchInstruction
		const inverseJumpMap = new Map<number, (number | null)[]>() //map of case starts to values which hit that case
		function addCase(value: number | null, offset: number) {
			const caseStart = firstSwitch + offset
			let caseValues = inverseJumpMap.get(caseStart)
			if (!caseValues) inverseJumpMap.set(caseStart, caseValues = [])
			caseValues.push(value)
		}
		for (const [value, offset] of offsetMap) addCase(value, offset)
		addCase(null, defaultOffset)
		const caseStarts = [...inverseJumpMap.keys()].sort((a, b) => a - b)
		if (Math.max(...caseStarts) > end) throw new Error('Switch statement exceeds block')
		const switchLabel = {label: `switch${loopCounter.count++}`}
		const cases: Case[] = []
		for (let caseIndex = 0; caseIndex < caseStarts.length; caseIndex++) {
			const caseStart = caseStarts[caseIndex]
			function parseCase(): Block {
				const caseEnd = caseIndex + 1 < caseStarts.length
					? caseStarts[caseIndex + 1]
					: controlResume === undefined ? end : controlResume
				try {
					const caseStack: Stack = []
					const caseBlock = parseControlFlow(
						instructions, exceptionTable,
						caseStart, caseEnd,
						loopCounter, breaks, continues,
						caseStack
					)
					if (caseStack.length) throw new Error('Expected empty case stack')
					return caseBlock
				}
				catch (e) {
					if (controlResume === undefined && e instanceof IfExceedsBoundsError) {
						const jumpIndex = preceding.get(e.instructionAfterIf)!
						const jump = instructions.get(jumpIndex)!.instruction
						if (jump instanceof Jump) {
							controlResume = jumpIndex + jump.offset
							if (controlResume <= firstSwitch || controlResume > end) throw new Error('Break exceeds block')
							breaks.set(controlResume, switchLabel)
							return parseCase() //try again with the break defined
						}
					}
					throw e
				}
			}
			const values = inverseJumpMap.get(caseStart)!
			const lastValue = values.pop() as number | null
			for (const emptyCaseLabel of values) {
				cases.push({
					exp: getCaseExpression(emptyCaseLabel),
					block: []
				})
			}
			cases.push({
				exp: getCaseExpression(lastValue),
				block: parseCase()
			})
		}
		block.push(new SwitchStatement(forcePop(stack), cases, switchLabel))
		if (controlResume !== undefined) breaks.delete(controlResume)
	}
	else if (firstControl === firstMonitor) {
		const obj = forcePop(stack)
		const synchronizedStart = firstMonitor + instructions.get(firstMonitor)!.length
		let retryCatch: ExceptionTableEntry | undefined
		const synchronizedCatch = exceptionTable.find(({startPC, handlerPC}) =>
			startPC === synchronizedStart && !!(retryCatch = exceptionTable.find(catcher =>
				catcher.startPC === handlerPC && catcher.handlerPC === handlerPC
			))
		)
		const revisedExceptionTable = exceptionTable.filter(catcher =>
			!(catcher === synchronizedCatch || catcher === retryCatch)
		)
		const synchronizedBlock: Block = []
		try {
			const synchronizedStack: Stack = []
			parseControlFlow(
				instructions, revisedExceptionTable,
				synchronizedStart, end,
				loopCounter, breaks, continues,
				synchronizedStack, synchronizedBlock
			)
			if (synchronizedStack.length) throw new Error('Expected empty stack')
		}
		catch (e) {
			if (!(e instanceof MonitorExitError)) throw e
			block.push(new SynchronizedStatement(obj, synchronizedBlock))
			const instructionLength = instructions.get(e.nextInstruction)
			if (instructionLength) {
				const {instruction} = instructionLength
				if (instruction instanceof Goto) {
					controlResume = e.nextInstruction + instruction.offset
				}
			}
			if (controlResume === undefined) throw new Error('Expected Goto after synchronized body')
		}
	}
	else if (firstControl === firstTryStart) {
		const revisedExceptionTable = exceptionTable.filter(({startPC, endPC}) =>
			!(startPC === firstTryStart && endPC === firstTryEnd)
		)
		const tryStack: Stack = []
		const tryBlock = parseControlFlow(
			instructions, revisedExceptionTable,
			firstTryStart, firstTryEnd,
			loopCounter, breaks, continues,
			tryStack
		)
		if (tryStack.length) throw new Error('Expected empty stack')
		function resolveExceptionStore(pc: number) {
			const {instruction, length} = instructions.get(pc)!
			if (!(instruction instanceof AStore)) throw new Error('Expected catch block to start with AStore')
			return {
				pc: pc + length,
				variable: new Variable(instruction.n)
			}
		}
		const finallyCatch = firstTryCatches!.find(({clazz}) => !clazz)
		if (finallyCatch) { //a finally clause
			const catchPC = finallyCatch.pc
			controlResume = end
			const beforeCatchIndex = preceding.get(catchPC)!
			if (beforeCatchIndex >= firstTryEnd) {
				const beforeCatch = instructions.get(beforeCatchIndex)!.instruction
				if (beforeCatch instanceof Goto) controlResume = beforeCatchIndex + beforeCatch.offset
			}
			const {pc, variable} = resolveExceptionStore(catchPC)
			const finallyStack: Stack = []
			const finallyBlock = parseControlFlow(
				instructions, revisedExceptionTable,
				pc, controlResume,
				loopCounter, breaks, continues,
				finallyStack
			)
			if (finallyStack.length) throw new Error('Expected empty stack')
			block.push(new TryStatement(
				tryBlock,
				[{types: [], variable, block: finallyBlock}]
			))
		}
		else { //not a finally clause
			const firstInstructionAfterTry = instructions.get(firstTryEnd)
			if (firstInstructionAfterTry) {
				const {instruction} = firstInstructionAfterTry
				if (instruction instanceof Goto) controlResume = firstTryEnd + instruction.offset
			}
			const handlerPCTypes = new Map<number, Class[]>()
			for (const {pc, clazz} of firstTryCatches) {
				let pcTypes = handlerPCTypes.get(pc)
				if (!pcTypes) handlerPCTypes.set(pc, pcTypes = [])
				pcTypes.push(clazz!)
			}
			const catches: Catch[] = []
			for (const [handlerPC, types] of handlerPCTypes) {
				const {pc, variable} = resolveExceptionStore(handlerPC)
				function parseCatch(): Block {
					let catchEnd: number
					if (controlResume === undefined) catchEnd = end
					else {
						catchEnd = controlResume
						for (let index = pc; index < catchEnd;) {
							const instructionLength = instructions.get(index)
							if (!instructionLength) throw new Error(`Missing instruction at ${index}`)
							const {instruction, length} = instructionLength
							if (instruction instanceof Goto) {
								const target = index + instruction.offset
								if (target === controlResume) catchEnd = index
							}
							index += length
						}
					}
					try {
						const catchStack: Stack = []
						const catchBlock = parseControlFlow(
							instructions, exceptionTable,
							pc, catchEnd,
							loopCounter, breaks, continues,
							catchStack
						)
						if (catchStack.length) throw new Error('Expected empty stack')
						return catchBlock
					}
					catch (e) {
						if (controlResume === undefined && e instanceof IfExceedsBoundsError) {
							const jumpIndex = preceding.get(e.instructionAfterIf)!
							const jump = instructions.get(jumpIndex)!.instruction
							if (jump instanceof Goto) {
								controlResume = jumpIndex + jump.offset
								if (controlResume <= pc || controlResume > end) throw new Error('End of catches exceeds block')
								return parseCatch() //try again with controlResume defined
							}
						}
						throw e
					}
				}
				catches.push({
					types: types.map(clazz => new ClassReference(clazz)),
					variable,
					block: parseCatch()
				})
			}
			block.push(new TryStatement(tryBlock, catches))
		}
	}
	if (controlResume !== undefined) {
		parseControlFlow(
			instructions, exceptionTable,
			controlResume, end,
			loopCounter, breaks, continues,
			stack, block
		)
	}
	return block
}
export function parseMethodAST(instructions: Code, exceptionTable: ExceptionTable): Block {
	let end = 0
	for (const [offset, {length}] of instructions) end = offset + length
	const stack: Stack = []
	const block = parseControlFlow(
		instructions, exceptionTable,
		0, end,
		{count: 1}, new Map, new Map,
		stack
	)
	if (stack.length) throw new Error('Expected empty stack')
	return block
}