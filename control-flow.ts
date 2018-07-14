import {
	BinaryOperation,
	Block,
	BooleanLiteral,
	BreakStatement,
	ContinueStatement,
	IfStatement,
	LoopReference,
	Ternary,
	UnaryOperation,
	WhileStatement
} from './ast'
import {Code, forcePop, Goto, If, Jump, Stack} from './bytecode-parser'

interface LoopCounter {
	count: number
}
class Loop implements LoopReference {
	constructor(public readonly n: number) {}
	get label() {
		return `loop${this.n}`
	}
}
type Loops = Map<number, Loop>

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

export function parseControlFlow(
	instructions: Code,
	start = 0, //inclusive
	end = Infinity, //exclusive
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
	    firstJumpBackSource: number //index of the do-while jump
	for (let index = start; index < end;) {
		const instructionLength = instructions.get(index)
		if (!instructionLength) break // end of method
		const {instruction, length} = instructionLength
		const nextIndex = index + length
		preceding.set(nextIndex, index)
		// Breaks and continues don't need to be processed as separate blocks
		if (instruction instanceof Jump && !(getBreak(index) || getContinue(index))) {
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
	const firstControl = Math.min(firstJumpForward, firstJumpBack, end)
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
	if (firstJumpForward < firstJumpBack && firstJumpForward <= end) {
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
	else if (firstJumpBack < Math.min(end, firstJumpForward)) {
		if (firstJumpBackSource! > end) throw new Error('While loop exceeds block')
		const loopEnd = firstJumpBackSource + instructions.get(firstJumpBackSource)!.length
		const loop = new Loop(loopCounter.count++)
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
	return block
}