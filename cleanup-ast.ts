import {
	BinaryOp,
	BinaryOperation,
	Block,
	BooleanLiteral,
	BreakStatement,
	Cast,
	ClassReference,
	ContinueStatement,
	Expression,
	ExpressionHandler,
	IfStatement,
	IntegerLiteral,
	NewArray,
	NewObject,
	replaceBlock,
	Replacements,
	ReturnStatement,
	Statement,
	StatementHandler,
	Ternary,
	UnaryOperation,
	WhileStatement
} from './ast'

const NEGATED_COMPARISONS = new Map<BinaryOp, BinaryOp>([
	['==', '!='],
	['!=', '=='],
	['<', '>='],
	['<=', '>'],
	['>', '<='],
	['>=', '<']
])

function walkBlockExpressions(block: Block, handler: ExpressionHandler) {
	for (const statement of block) statement.walkExpressions(handler)
}
function walkBlockStatements(block: Block, handler: StatementHandler) {
	for (const statement of block) statement.walkStatements(handler)
}
const isTrue = (cond: Expression) =>
	(cond instanceof BooleanLiteral && cond.b) ||
	(cond instanceof IntegerLiteral && String(cond.i) === '1') //string comparison since it might be a BigInt
const isFalse = (cond: Expression) =>
	(cond instanceof BooleanLiteral && !cond.b) ||
	(cond instanceof IntegerLiteral && !cond.i)

type CleanupStrategy = (block: Block) => Replacements

const removeTrailingReturn: CleanupStrategy = block => {
	const replacements = new Map<Statement, Statement[]>()
	const [lastStatement] = block.slice(-1)
	if (lastStatement instanceof ReturnStatement && !lastStatement.exp) {
		replacements.set(lastStatement, [])
	}
	return {
		expressions: new Map,
		statements: replacements
	}
}
const avoidEmptyIfBlock: CleanupStrategy = block => {
	const replacements = new Map<Statement, Statement[]>()
	walkBlockStatements(block, statement => {
		if (statement instanceof IfStatement && !statement.ifBlock.length) {
			replacements.set(statement, [new IfStatement(
				new UnaryOperation('!', statement.cond),
				statement.elseBlock,
				statement.ifBlock
			)])
		}
	})
	return {
		expressions: new Map,
		statements: replacements
	}
}
const avoidNegation: CleanupStrategy = block => {
	const replacements = new Map<Expression, Expression>()
	walkBlockExpressions(block, expression => {
		if (expression instanceof UnaryOperation && expression.op === '!') {
			const {arg} = expression
			let replacement: Expression | undefined
			if (arg instanceof UnaryOperation && arg.op === '!') {
				replacement = arg.arg
			}
			else if (arg instanceof BinaryOperation) {
				const newOp = NEGATED_COMPARISONS.get(arg.op)
				if (newOp) replacement = new BinaryOperation(newOp, arg.arg1, arg.arg2)
			}
			if (replacement) replacements.set(expression, replacement)
		}
	})
	return {
		expressions: replacements,
		statements: new Map
	}
}
//TODO: this isn't smart enough to distinguish between cond ? true : false and cond ? 1 : 0
const resolveBooleanTernary: CleanupStrategy = block => {
	const replacements = new Map<Expression, Expression>()
	walkBlockExpressions(block, expression => {
		if (expression instanceof Ternary) {
			const {cond, ifTrue, ifFalse} = expression
			if (isTrue(ifTrue) && isFalse(ifFalse)) replacements.set(expression, cond)
			else if (isFalse(ifTrue) && isTrue(ifFalse)) replacements.set(expression, new UnaryOperation('!', cond))
		}
	})
	return {
		expressions: replacements,
		statements: new Map
	}
}
const avoidSubtractionCmp: CleanupStrategy = block => {
	const replacements = new Map<Expression, Expression>()
	walkBlockExpressions(block, expression => {
		if (expression instanceof BinaryOperation) {
			const {op: cmpOp, arg1, arg2} = expression
			if (NEGATED_COMPARISONS.has(cmpOp)) {
				if (arg2 instanceof IntegerLiteral && !arg2.i) {
					if (arg1 instanceof BinaryOperation) {
						const {op, arg1: left, arg2: right} = arg1
						if (op === '-') {
							replacements.set(expression, new BinaryOperation(cmpOp, left, right))
						}
					}
				}
			}
		}
	})
	return {
		expressions: replacements,
		statements: new Map
	}
}
const avoidDoubleCast: CleanupStrategy = block => {
	const replacements = new Map<Expression, Expression>()
	walkBlockExpressions(block, expression => {
		if (expression instanceof Cast) {
			const {type, exp, primitive} = expression
			if (exp instanceof Cast) {
				replacements.set(expression, new Cast(type, exp.exp, primitive))
			}
		}
	})
	return {
		expressions: replacements,
		statements: new Map
	}
}
const identifyDoWhileCondition: CleanupStrategy = block => {
	const replacements = new Map<Statement, Statement[]>()
	walkBlockStatements(block, statement => {
		if (statement instanceof WhileStatement) {
			const {doWhile, cond, block, label} = statement
			if (doWhile && isFalse(cond)) {
				const [lastStatement] = block.slice(-1)
				let doWhileCond: Expression | undefined
				if (lastStatement instanceof IfStatement) {
					const {cond, ifBlock, elseBlock} = lastStatement
					const [firstIfStatement] = ifBlock
					if (firstIfStatement instanceof ContinueStatement && firstIfStatement.loop === label && !elseBlock.length) {
						doWhileCond = cond
					}
				}
				if (!doWhileCond) {
					if (lastStatement instanceof ContinueStatement && lastStatement.loop === label) {
						doWhileCond = new BooleanLiteral(true)
					}
				}
				if (doWhileCond) {
					replacements.set(statement, [new WhileStatement(
						doWhileCond,
						block.slice(0, -1), //leave out if (cond) continue;
						true,
						label
					)])
				}
			}
		}
	})
	return {
		expressions: new Map,
		statements: replacements
	}
}
const trueDoWhileToWhile: CleanupStrategy = block => {
	const replacements = new Map<Statement, Statement[]>()
	walkBlockStatements(block, statement => {
		if (statement instanceof WhileStatement) {
			const {cond, block, doWhile, label} = statement
			if (doWhile && isTrue(cond)) {
				replacements.set(statement, [new WhileStatement(
					cond,
					block,
					false,
					label
				)])
			}
		}
	})
	return {
		expressions: new Map,
		statements: replacements
	}
}
const identifyWhileCondition: CleanupStrategy = block => {
	const replacements = new Map<Statement, Statement[]>()
	walkBlockStatements(block, statement => {
		if (statement instanceof WhileStatement) {
			const {doWhile, cond, block, label} = statement
			if (!doWhile && isTrue(cond)) {
				const [firstStatement, ...body] = block
				if (firstStatement instanceof IfStatement) {
					const {cond, ifBlock, elseBlock} = firstStatement
					const [firstIfStatement] = ifBlock
					if (firstIfStatement instanceof BreakStatement && firstIfStatement.loop === label && !elseBlock.length) {
						replacements.set(statement, [new WhileStatement(
							new UnaryOperation('!', cond),
							body, //leave out if (cond) break;
							false,
							label
						)])
					}
				}
			}
		}
	})
	return {
		expressions: new Map,
		statements: replacements
	}
}
const resolveTrueIfCondition: CleanupStrategy = block => {
	const replacements = new Map<Statement, Statement[]>()
	walkBlockStatements(block, statement => {
		if (statement instanceof IfStatement) {
			const {cond, elseBlock} = statement
			if (isTrue(cond) && !elseBlock.length) {
				replacements.set(statement, statement.ifBlock)
			}
		}
	})
	return {
		expressions: new Map,
		statements: replacements
	}
}
const resolveIfBodyOfWhile: CleanupStrategy = block => {
	const replacements = new Map<Statement, Statement[]>()
	walkBlockStatements(block, statement => {
		if (statement instanceof WhileStatement) {
			const {cond, block, label} = statement
			if (isFalse(cond) && block.length === 1) {
				const [statement] = block
				if (statement instanceof IfStatement) {
					const {cond, ifBlock, elseBlock} = statement
					if (!elseBlock.length) {
						replacements.set(statement, [
							new IfStatement(
								new UnaryOperation('!', cond),
								[new BreakStatement(label)],
							),
							...ifBlock
						])
					}
				}
			}
		}
	})
	return {
		expressions: new Map,
		statements: replacements
	}
}
const STRATEGIES = [
	removeTrailingReturn,
	avoidEmptyIfBlock,
	avoidNegation,
	resolveBooleanTernary,
	avoidSubtractionCmp,
	avoidDoubleCast,
	identifyDoWhileCondition,
	trueDoWhileToWhile,
	identifyWhileCondition,
	resolveTrueIfCondition,
	resolveIfBodyOfWhile
]

export function cleanup(block: Block): Block {
	let someReplaced: boolean
	do {
		someReplaced = false
		for (const strategy of STRATEGIES) {
			const replacements = strategy(block)
			if (replacements.expressions.size || replacements.statements.size) {
				block = replaceBlock(block, replacements)
				someReplaced = true
			}
		}
	} while (someReplaced)
	return block
}
//Replaces, e.g. java/util/ArrayList with ArrayList
export function resolvePackageClasses(block: Block, imports: Set<string>): Block {
	function convertClassString(clazz: string) {
		const dotString = clazz.replace(/\//g, '.')
		if (!dotString.startsWith('java.lang.')) imports.add(dotString)
		const [clazzName] = dotString.split('.').slice(-1)
		return clazzName
	}
	const replacements = new Map<Expression, Expression>()
	walkBlockExpressions(block, expression => {
		if (expression instanceof ClassReference) {
			replacements.set(expression, new ClassReference({
				name: convertClassString(expression.clazz.name)
			}))
		}
		else if (expression instanceof NewObject) {
			const replacedExpression = new NewObject({
				name: convertClassString(expression.clazz.name)
			})
			replacedExpression.args = expression.args
			replacements.set(expression, replacedExpression)
		}
		else if (expression instanceof NewArray && !expression.primitive) {
			replacements.set(expression, new NewArray(
				{name: convertClassString(expression.type.name)},
				expression.length,
				expression.primitive
			))
		}
		else if (expression instanceof Cast && !expression.primitive) {
			replacements.set(expression, new Cast(
				{name: convertClassString(expression.type.name)},
				expression.exp,
				expression.primitive
			))
		}
	})
	return replaceBlock(block, {expressions: replacements, statements: new Map})
}