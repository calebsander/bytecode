import {
	BinaryOperation,
	Block,
	BooleanLiteral,
	BreakStatement,
	Case,
	ContinueStatement,
	IfStatement,
	IntegerLiteral,
	LoopReference,
	Ternary,
	UnaryOperation,
	WhileStatement,
	SwitchStatement
} from './ast'
import {Code, forcePop, Goto, If, Jump, Stack, TableSwitch} from './bytecode-parser'

interface LoopCounter {
	count: number
}
type Loops = Map<number, LoopReference>

function equalsUpTo<T>(arr1: T[], arr2: T[], len: number) {
	for (let i = 0; i < len; i++) {
		if (arr1[i] !== arr2[i]) return false
	}
	return true
}

class IfExceedsBoundsError extends Error {
	constructor(public readonly instructionAfterIf: number) {
		super('If statement exceeds block')
	}
}

const getCaseExpression = (value: number | null) =>
	value === null ? null : new IntegerLiteral(value)

export function parseControlFlow(
	instructions: Code,
	start = 0, //inclusive
	end = Math.max(...[...instructions].map(([offset, {length}]) => offset + length)), //exclusive
	loopCounter: LoopCounter = {count: 1},
	breaks: Loops = new Map,
	continues: Loops = new Map,
	stack: Stack = [],
	block: Block = []
): Block {
	const getBreak = (index: number) => {
		const {instruction} = instructions.get(index)!
		if (instruction instanceof Goto || instruction instanceof If) {
			const jumpTarget = index + instruction.offset
			return breaks.get(jumpTarget)
		}
		return undefined
	}
	const getContinue = (index: number) => {
		const {instruction} = instructions.get(index)!
		if (instruction instanceof Goto || instruction instanceof If) {
			const jumpTarget = index + instruction.offset
			return continues.get(jumpTarget)
		}
		return undefined
	}
	const preceding = new Map<number, number>()
	let firstJumpForward = Infinity, //index of the instruction after the if jump
	    firstJumpForwardTarget: number, //index of the start of the else block, or after the if statement
	    firstJumpBack = Infinity, //index of the target of the do-while jump back
	    firstJumpBackSource: number, //index of the do-while jump
	    firstSwitch = Infinity //index of tableswitch instruction
	for (let index = start; index < end;) {
		const instructionLength = instructions.get(index)
		if (!instructionLength) break // end of method
		const {instruction, length} = instructionLength
		const nextIndex = index + length
		preceding.set(nextIndex, index)
		if (instruction instanceof TableSwitch) firstSwitch = Math.min(firstSwitch, index)
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
	const firstControl = Math.min(firstJumpForward, firstJumpBack, firstSwitch, end)
	for (let index = start; index < firstControl;) {
		const instructionLength = instructions.get(index)
		if (!instructionLength) break // end of method
		const {instruction, length} = instructionLength
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
	if (firstControl === firstJumpForward) {
		if (firstJumpForwardTarget! > end) throw new IfExceedsBoundsError(firstJumpForward)
		const elseCondition = forcePop(stack)
		const lastIndexOfIf = preceding.get(firstJumpForwardTarget)!
		const lastInstructionOfIf = instructions.get(lastIndexOfIf)!.instruction
		const hasElseBlock = lastInstructionOfIf instanceof Goto &&
			!(getBreak(lastIndexOfIf) || getContinue(lastIndexOfIf))
		let ifBlockEnd = hasElseBlock ? lastIndexOfIf : firstJumpForwardTarget
		let elseBlockEnd: number, elseBlock: Block, elseStack: Stack
		if (hasElseBlock) {
			elseBlockEnd = lastIndexOfIf + (lastInstructionOfIf as Goto).offset
			elseStack = [...stack]
			elseBlock = parseControlFlow(
				instructions,
				firstJumpForwardTarget, elseBlockEnd,
				loopCounter, breaks, continues,
				elseStack
			)
		}
		else elseBlock = elseStack = []
		const ifStack = [...stack]
		const ifBlock: Block = []
		try {
			parseControlFlow(
				instructions,
				firstJumpForward, ifBlockEnd,
				loopCounter, breaks, continues,
				ifStack, ifBlock
			)
			const isTernary = !(ifBlock.length || elseBlock.length) &&
				ifStack.length === stack.length + 1 && equalsUpTo(ifStack, stack, stack.length) &&
				elseStack.length === stack.length + 1 && equalsUpTo(elseStack, stack, stack.length)
			const cond = new UnaryOperation('!', elseCondition)
			if (isTernary) stack.push(new Ternary(cond, ifStack.pop()!, elseStack.pop()!))
			else block.push(new IfStatement(cond, ifBlock, elseBlock))
		}
		catch (e) {
			let isAnd = false, isOr = false
			if (e instanceof IfExceedsBoundsError && !ifBlock.length && ifStack.length === stack.length + 1) {
				if (!elseBlock.length && elseStack.length === 1) {
					const andStack: Stack = []
					const andBlock = parseControlFlow(
						instructions,
						e.instructionAfterIf, ifBlockEnd,
						loopCounter, breaks, continues,
						andStack
					)
					if (!andBlock.length && andStack.length === 1) {
						stack.push(new Ternary(
							new BinaryOperation(
								'&&',
								new UnaryOperation('!', elseCondition),
								new UnaryOperation('!', ifStack.pop()!)
							),
							andStack[0],
							elseStack[0]
						))
						isAnd = true
					}
				}
				if (!isAnd) {
					if (e.instructionAfterIf === ifBlockEnd && !hasElseBlock) {
						const cond2JumpIndex = preceding.get(ifBlockEnd)!
						const cond2Jump = instructions.get(cond2JumpIndex)!.instruction
						if (cond2Jump instanceof If) {
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
				}
			}
			if (!(isAnd || isOr)) throw e
		}
		const newStart = hasElseBlock ? elseBlockEnd! : ifBlockEnd
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
		const {offsetMap, defaultOffset} = instructions.get(firstSwitch)!.instruction as TableSwitch
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
			for (const emptyCaseLabel of values.slice(0, -1)) {
				cases.push({
					exp: getCaseExpression(emptyCaseLabel),
					block: []
				})
			}
			const [lastValue] = values.slice(-1)
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
	return block
}