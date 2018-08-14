import {
	BinaryOperation,
	Block,
	BooleanLiteral,
	BreakStatement,
	Case,
	ContinueStatement,
	Expression,
	IfStatement,
	IntegerLiteral,
	LoopReference,
	Ternary,
	UnaryOperation,
	WhileStatement,
	SwitchStatement,
	SynchronizedStatement
} from './ast'
import {Code} from './bytecode-parser'
import {
	forcePop,
	Goto,
	IfCondition,
	Jump,
	MonitorEnter,
	MonitorExitError,
	Stack,
	SwitchInstruction
} from './instructions'

interface LoopCounter {
	count: number
}
type Loops = Map<number, LoopReference>

class IfExceedsBoundsError extends Error {
	constructor(
		public readonly instructionAfterIf: number,
		public readonly target: number
	) {
		super('If statement exceeds block')
	}
}

const getCaseExpression = (value: number | null) =>
	value === null ? null : new IntegerLiteral(value)

function parseControlFlow(
	instructions: Code,
	start: number, //inclusive
	end: number, //exclusive
	loopCounter: LoopCounter,
	breaks: Loops,
	continues: Loops,
	stack: Stack,
	block: Block = []
): Block {
	const getBreak = (index: number) => {
		const {instruction} = instructions.get(index)!
		return instruction instanceof Goto || instruction instanceof IfCondition
			? breaks.get(index + instruction.offset)
			: undefined
	}
	const getContinue = (index: number) => {
		const {instruction} = instructions.get(index)!
		return instruction instanceof Goto || instruction instanceof IfCondition
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
			for (const offset of instruction.offsets) {
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
		}
		index = nextIndex
	}
	const firstControl = Math.min(
		firstJumpForward,
		firstJumpBack,
		firstSwitch,
		firstMonitor,
		end
	)
	for (let index = start; index < firstControl;) {
		const instructionLength = instructions.get(index)
		if (!instructionLength) throw new Error(`Missing instruction at ${index}`)
		const {instruction, length} = instructionLength
		try { instruction.execute(stack, block) }
		catch (e) {
			if (e instanceof MonitorExitError) e = new MonitorExitError(index + length)
			throw e
		}
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
	if (firstControl === firstJumpForward) {
		if (firstJumpForwardTarget! > end) throw new IfExceedsBoundsError(firstJumpForward, firstJumpForwardTarget)
		const elseCondition = forcePop(stack)
		const lastIndexOfIf = preceding.get(firstJumpForwardTarget)!
		const lastInstructionOfIf = instructions.get(lastIndexOfIf)!.instruction
		const hasElseBlock = lastInstructionOfIf instanceof Goto &&
			!(getBreak(lastIndexOfIf) || getContinue(lastIndexOfIf))
		let ifBlockEnd = hasElseBlock ? lastIndexOfIf : firstJumpForwardTarget
		let elseBlockEnd: number | undefined, elseBlock: Block = [], elseStack: Stack = []
		if (hasElseBlock) {
			elseBlockEnd = lastIndexOfIf + (lastInstructionOfIf as Goto).offset
			parseControlFlow(
				instructions,
				firstJumpForwardTarget, elseBlockEnd,
				loopCounter, breaks, continues,
				elseStack, elseBlock
			)
		}
		if (!elseBlock.length && elseStack.length === 1) { //&& or ternary
			const elseConditions = [elseCondition] //contains expressions causing jumps to else block
			let ifResult: Expression | undefined
			let clauseStart = firstJumpForward
			do {
				const clauseStack: Stack = []
				const clauseBlock: Block = []
				try {
					parseControlFlow(
						instructions,
						clauseStart, ifBlockEnd,
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
		else {
			const ifStack: Stack = []
			const ifBlock: Block = []
			try {
				parseControlFlow(
					instructions,
					firstJumpForward, ifBlockEnd,
					loopCounter, breaks, continues,
					ifStack, ifBlock
				)
				block.push(new IfStatement(new UnaryOperation('!', elseCondition), ifBlock, elseBlock))
			}
			catch (e) {
				let isOr = false
				if (e instanceof IfExceedsBoundsError && e.instructionAfterIf === ifBlockEnd && !ifBlock.length && ifStack.length === 1 && !hasElseBlock) {
					const cond2JumpIndex = preceding.get(ifBlockEnd)!
					const cond2Jump = instructions.get(cond2JumpIndex)!.instruction
					if (cond2Jump instanceof IfCondition) {
						const falseIndex = cond2JumpIndex + cond2Jump.offset
						const gotoEndIndex = preceding.get(falseIndex)!
						const gotoEnd = instructions.get(gotoEndIndex)!.instruction
						if (gotoEnd instanceof Goto) {
							const trueStack: Stack = []
							const trueBlock = parseControlFlow(
								instructions,
								ifBlockEnd, gotoEndIndex,
								loopCounter, breaks, continues,
								trueStack
							)
							if (!trueBlock.length && trueStack.length === 1) {
								const endIndex = gotoEndIndex + gotoEnd.offset
								const falseStack: Stack = []
								const falseBlock = parseControlFlow(
									instructions,
									falseIndex, endIndex,
									loopCounter, breaks, continues,
									falseStack
								)
								if (!falseBlock.length && falseStack.length === 1) {
									stack.push(new Ternary(
										new BinaryOperation('||', elseCondition, new UnaryOperation('!', ifStack[0])),
										trueStack[0],
										falseStack[0]
									))
									ifBlockEnd = endIndex
									isOr = true
								}
							}
						}
					}
				}
				if (!isOr) throw e
			}
		}
		const newStart = elseBlockEnd || ifBlockEnd
		parseControlFlow(
			instructions,
			newStart, end,
			loopCounter, breaks, continues,
			stack, block
		)
	}
	else if (firstControl === firstJumpBack) {
		if (firstJumpBackSource! > end) throw new Error('While loop exceeds block')
		const loopEnd = firstJumpBackSource + instructions.get(firstJumpBackSource)!.length
		const loop = {label: `loop${loopCounter.count++}`}
		breaks.set(loopEnd, loop)
		continues.set(firstJumpBack, loop)
		const whileStack: Stack = []
		const whileBlock = parseControlFlow(
			instructions,
			firstJumpBack, loopEnd,
			loopCounter, breaks, continues,
			whileStack
		)
		if (whileStack.length) throw new Error('Expected empty while stack')
		breaks.delete(loopEnd)
		continues.delete(firstJumpBack)
		block.push(new WhileStatement(
			new BooleanLiteral(false),
			whileBlock,
			true,
			loop
		))
		parseControlFlow(
			instructions,
			loopEnd, end,
			loopCounter, breaks, continues,
			stack, block
		)
	}
	else if (firstControl === firstSwitch) {
		const {offsetMap, defaultOffset} = instructions.get(firstSwitch)!.instruction as SwitchInstruction
		const caseStartSet = new Set<number>()
		const inverseJumpMap = new Map<number, (number | null)[]>() //map of case starts to values which hit that case
		const offsetMapWithDefault = new Map<number | null, number>(offsetMap)
			.set(null, defaultOffset)
		for (const [value, offset] of offsetMapWithDefault) {
			const caseStart = firstSwitch + offset
			caseStartSet.add(caseStart)
			let caseValues = inverseJumpMap.get(caseStart)
			if (!caseValues) {
				caseValues = []
				inverseJumpMap.set(caseStart, caseValues)
			}
			caseValues.push(value)
		}
		const caseStarts = [...caseStartSet].sort((a, b) => a - b)
		if (Math.max(...caseStarts) > end) throw new Error('Switch statement exceeds block')
		const val = forcePop(stack)
		const switchLabel = {label: `switch${loopCounter.count++}`}
		let breakIndex: number | undefined //guaranteed to be nonzero if defined
		const caseBlocks = new Map<number, Block>()
		for (let caseIndex = 0; caseIndex < caseStarts.length; caseIndex++) {
			const caseStart = caseStarts[caseIndex]
			function parseCase(): Block {
				const caseEnd = caseIndex + 1 < caseStarts.length
					? caseStarts[caseIndex + 1]
					: breakIndex || end
				try {
					const caseStack: Stack = []
					const caseBlock = parseControlFlow(
						instructions,
						caseStart, caseEnd,
						loopCounter, breaks, continues,
						caseStack
					)
					if (caseStack.length) throw new Error('Expected empty case stack')
					return caseBlock
				}
				catch (e) {
					if (!breakIndex && e instanceof IfExceedsBoundsError) {
						const jumpIndex = preceding.get(e.instructionAfterIf)!
						const jump = instructions.get(jumpIndex)!.instruction
						if (jump instanceof Goto) {
							breakIndex = jumpIndex + jump.offset
							if (breakIndex <= firstSwitch || breakIndex > end) throw new Error('Break exceeds block')
							breaks.set(breakIndex, switchLabel)
							return parseCase() //try again with the break defined
						}
					}
					throw e
				}
			}
			caseBlocks.set(caseStart, parseCase())
		}
		const cases: Case[] = []
		for (const caseStart of caseStarts) {
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
				block: caseBlocks.get(caseStart)!
			})
		}
		block.push(new SwitchStatement(val, cases, switchLabel))
		if (breakIndex !== undefined) {
			breaks.delete(breakIndex)
			parseControlFlow(
				instructions,
				breakIndex, end,
				loopCounter, breaks, continues,
				stack, block
			)
		}
	}
	else if (firstControl === firstMonitor) {
		const obj = forcePop(stack)
		const synchronizedStart = firstMonitor + instructions.get(firstMonitor)!.length
		const synchronizedStack: Stack = []
		const synchronizedBlock: Block = []
		try {
			parseControlFlow(
				instructions,
				synchronizedStart, end,
				loopCounter, breaks, continues,
				synchronizedStack, synchronizedBlock
			)
		}
		catch (e) {
			if (!(e instanceof MonitorExitError && e.nextInstruction)) throw e
			block.push(new SynchronizedStatement(obj, synchronizedBlock))
			const instructionLength = instructions.get(e.nextInstruction)
			let instructionAfterCatch: number | undefined
			if (instructionLength) {
				const {instruction} = instructionLength
				if (instruction instanceof Goto) {
					instructionAfterCatch = e.nextInstruction + instruction.offset
				}
			}
			if (!instructionAfterCatch) throw new Error('Expected Goto after synchronized body')
			parseControlFlow(
				instructions,
				instructionAfterCatch, end,
				loopCounter, breaks, continues,
				stack, block
			)
		}
	}
	return block
}
export function parseMethodAST(instructions: Code): Block {
	let end = 0
	for (const [offset, {length}] of instructions) end = offset + length
	return parseControlFlow(
		instructions,
		0, end,
		{count: 1}, new Map, new Map,
		[]
	)
}