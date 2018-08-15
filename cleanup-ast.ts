import {
	Assignment,
	AssignmentBinaryOp,
	ASSIGNMENT_BINARY_OPS,
	BinaryOp,
	BinaryOperation,
	Block,
	BlockHandler,
	BooleanLiteral,
	BreakStatement,
	Case,
	Cast,
	ClassLiteral,
	ClassReference,
	ContinueStatement,
	Expression,
	ExpressionStatement,
	ExpressionHandler,
	FunctionCall,
	IfStatement,
	IntegerLiteral,
	NewArray,
	NewObject,
	replaceBlock,
	Replacements,
	ReturnStatement,
	Statement,
	StatementHandler,
	StringLiteral,
	SwitchStatement,
	Ternary,
	UnaryOperation,
	Variable,
	WhileStatement
} from './ast'

const BOOLEAN = 'boolean'
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
	walkBlocks(block, block => {
		for (const statement of block) handler(statement)
	})
}
function walkBlocks(block: Block, handler: BlockHandler) {
	handler(block)
	for (const statement of block) statement.walkBlocks(handler)
}
const isTrue = (cond: Expression) =>
	cond instanceof BooleanLiteral && cond.b
const isFalse = (cond: Expression) =>
	cond instanceof BooleanLiteral && !cond.b

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
const collapseCasesWithDefault: CleanupStrategy = block => {
	const replacements = new Map<Statement, Statement[]>()
	walkBlockStatements(block, statement => {
		if (statement instanceof SwitchStatement) {
			const {val, cases, label} = statement
			const defaultIndex = cases.findIndex(({exp}) => !exp)
			if (defaultIndex > -1) {
				let firstEmptyIndex: number
				for (firstEmptyIndex = defaultIndex - 1; !cases[firstEmptyIndex].block.length; firstEmptyIndex--);
				firstEmptyIndex++;
				if (firstEmptyIndex < defaultIndex) {
					replacements.set(statement, [new SwitchStatement(
						val,
						cases.slice(0, firstEmptyIndex).concat(cases.slice(defaultIndex)),
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
const removeEmptyLastCase: CleanupStrategy = block => {
	const replacements = new Map<Statement, Statement[]>()
	walkBlockStatements(block, statement => {
		if (statement instanceof SwitchStatement) {
			const {val, cases, label} = statement
			let removeFrom = cases.length
			while (removeFrom && !cases[removeFrom - 1].block.length) removeFrom--
			if (removeFrom < cases.length) {
				replacements.set(statement, [new SwitchStatement(
					val,
					cases.slice(0, removeFrom),
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
const identifyStringSwitch: CleanupStrategy = block => {
	const replacements = new Map<Statement, Statement[]>()
	walkBlocks(block, block => {
		testIndex: for (let i = 0; i < block.length; i++) {
			const [valAssignment, indexInit, hashSwitch, indexSwitch] = block.slice(i, i + 4)
			if (!(valAssignment instanceof ExpressionStatement && valAssignment.exp instanceof Assignment &&
				valAssignment.exp.lhs instanceof Variable && indexInit instanceof ExpressionStatement && indexInit.exp instanceof Assignment)) continue
			const valVariableN = valAssignment.exp.lhs.n
			const {lhs, rhs} = indexInit.exp
			if (!(rhs instanceof IntegerLiteral && rhs.i === -1 && lhs instanceof Variable && hashSwitch instanceof SwitchStatement)) continue
			const indexVariableN = lhs.n
			// TODO: handle strings with hash collisions
			const indexValues = new Map<number, StringLiteral>()
			const {val, cases, label} = hashSwitch
			if (!(val instanceof FunctionCall)) continue
			const {obj, func, args} = val
			if (!(obj instanceof Variable && obj.n === valVariableN && func && func.name === 'hashCode' && !args.length)) continue
			for (let j = 0; j < cases.length; j++) {
				const {exp, block} = cases[j]
				if (!exp) continue testIndex
				const [ifEquals, setIndex, breakStatement] = block
				if (j < cases.length - 1) {
					if (!(block.length === 3 && breakStatement instanceof BreakStatement && breakStatement.loop === label)) continue testIndex
				}
				else if (block.length !== 2) continue testIndex
				if (!(ifEquals instanceof IfStatement)) continue testIndex
				const {cond, ifBlock, elseBlock} = ifEquals
				const [ifBreakStatement] = ifBlock
				if (!(cond instanceof BinaryOperation && ifBlock.length === 1 && ifBreakStatement instanceof BreakStatement &&
					ifBreakStatement.loop === label && !elseBlock.length)) continue testIndex
				const {op, arg1, arg2} = cond
				// TODO: 'equals() == 0' will need to be changed to '!equals()'
				if (!(op === '==' && arg1 instanceof FunctionCall && arg2 instanceof IntegerLiteral && !arg2.i && arg1)) continue testIndex
				const {obj, func, args} = arg1
				if (!(obj instanceof Variable && obj.n === valVariableN && func && func.name === 'equals' && args.length === 1)) continue testIndex
				const [str] = args
				if (!(str instanceof StringLiteral && setIndex instanceof ExpressionStatement && setIndex.exp instanceof Assignment)) continue testIndex
				const {lhs, rhs} = setIndex.exp
				if (!(lhs instanceof Variable && lhs.n === indexVariableN && rhs instanceof IntegerLiteral)) continue testIndex
				indexValues.set(rhs.i, str)
			}
			if (!(indexSwitch instanceof SwitchStatement && indexSwitch.val instanceof Variable && indexSwitch.val.n === indexVariableN)) continue
			const newCases: Case[] = []
			for (const {exp, block} of indexSwitch.cases) {
				if (exp) {
					if (!(exp instanceof IntegerLiteral)) continue testIndex
					const str = indexValues.get(exp.i)
					if (!str) continue testIndex
					newCases.push({exp: str, block})
				}
				else newCases.push({exp, block})
			}
			replacements
				.set(valAssignment, [])
				.set(indexInit, [])
				.set(hashSwitch, [])
				.set(indexSwitch, [new SwitchStatement(
					valAssignment.exp.rhs,
					newCases,
					indexSwitch.label
				)])
		}
	})
	return {
		expressions: new Map,
		statements: replacements
	}
}
const shorthandAssignments: CleanupStrategy = block => {
	const replacements = new Map<Expression, Expression>()
	walkBlockExpressions(block, expression => {
		if (expression instanceof Assignment) {
			const {lhs, rhs, op} = expression
			if (!op && rhs instanceof BinaryOperation) {
				const {op, arg1, arg2} = rhs
				if (op in ASSIGNMENT_BINARY_OPS && (lhs as Expression).toString(true) === arg1.toString(true)) {
					let incExpression: Expression | undefined
					if (op === '+' && arg2 instanceof IntegerLiteral) {
						const arg2String = `${arg2}`
						if (arg2String === '1') incExpression = new UnaryOperation({op: '++', post: false}, lhs)
						else if (arg2String === '-1') incExpression = new UnaryOperation({op: '--', post: false}, lhs)
					}
					replacements.set(
						expression,
						incExpression || new Assignment(lhs, arg2, op as AssignmentBinaryOp)
					)
				}
			}
		}
	})
	return {
		expressions: replacements,
		statements: new Map
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
	resolveIfBodyOfWhile,
	collapseCasesWithDefault,
	removeEmptyLastCase,
	identifyStringSwitch,
	shorthandAssignments
]

function walkBooleanContextExpressions(block: Block, notBooleanVars: Set<number>, returnBoolean: boolean, handler: ExpressionHandler) {
	//Process an expression which has been inferred by context to be boolean
	const processBooleanExpression: ExpressionHandler = expression => {
		handler(expression)
		if (expression instanceof UnaryOperation) {
			if (expression.op === '!') processBooleanExpression(expression.arg)
		}
		else if (expression instanceof BinaryOperation) {
			const {op, arg1, arg2} = expression
			switch (op) {
				case '&&':
				case '||':
				case '&':
				case '|':
				case '^':
				case '==':
				case '!=':
					//Need to verify types of arguments since == and !=
					//can be applied to non-booleans and still return boolean result
					if (arg1.maybeBoolean(notBooleanVars) && arg2.maybeBoolean(notBooleanVars)) {
						processBooleanExpression(arg1)
						processBooleanExpression(arg2)
					}
			}
		}
		else if (expression instanceof Ternary) {
			const {ifTrue, ifFalse} = expression
			processBooleanExpression(ifTrue)
			processBooleanExpression(ifFalse)
		}
	}
	//Find expressions known to be boolean; these are:
	walkBlockExpressions(block, expression => {
		if (expression instanceof Assignment) {
			const {lhs, rhs} = expression
			//Something that gets a boolean assigned to it
			if (rhs.maybeBoolean(notBooleanVars)) processBooleanExpression(lhs)
			//Something assigned to a boolean
			if ((lhs as Expression).maybeBoolean(notBooleanVars)) processBooleanExpression(rhs)
		}
		else if (expression instanceof FunctionCall) {
			const {args, argTypes} = expression
			for (let i = 0; i < args.length; i++) {
				//Something passed to a function as a boolean argument
				if (argTypes[i] === BOOLEAN) processBooleanExpression(args[i])
			}
		}
		//Something used as a ternary condition
		else if (expression instanceof Ternary) processBooleanExpression(expression.cond)
	})
	walkBlockStatements(block, statement => {
		//Something used as an if or while condition
		if (statement instanceof IfStatement || statement instanceof WhileStatement) {
			processBooleanExpression(statement.cond)
		}
		//Something returned from a function which returns booleans
		else if (returnBoolean && statement instanceof ReturnStatement) {
			const {exp} = statement
			if (!exp) throw new Error('Expected non-void return')
			processBooleanExpression(exp)
		}
	})
}
function getNotBooleanVars(block: Block, argTypes: Map<number, string>, localTypes: Map<number, string>, returnBoolean: boolean) {
	const notBooleanVars = new Set<number>()
	//Boolean argument types are explicitly boolean (inferred from descriptor)
	for (const [n, type] of argTypes) {
		if (type !== BOOLEAN) notBooleanVars.add(n)
	}
	//Boolean local types are only known to be ints (inferred from load/store instructions)
	for (const [n, type] of localTypes) {
		if (type !== 'int') notBooleanVars.add(n)
	}
	//Continue using new information to infer variables as non-booleans until no new information is available
	let initialSize: number
	do {
		initialSize = notBooleanVars.size
		//Count number of instances of each variable
		const timesSeen = new Map<number, number>()
		walkBlockExpressions(block, expression => {
			if (expression instanceof Variable) {
				const {n} = expression
				if (localTypes.has(n)) timesSeen.set(n, (timesSeen.get(n) || 0) + 1)
			}
		})
		//Count number of times variable is used possibly as a boolean
		const timesSeenCorrectly = new Map<number, number>()
		walkBooleanContextExpressions(block, notBooleanVars, returnBoolean, expression => {
			if (expression instanceof Variable) {
				const {n} = expression
				if (localTypes.has(n)) {
					timesSeenCorrectly.set(n, (timesSeenCorrectly.get(n) || 0) + 1)
				}
			}
		})
		//Reject variables which are definitely used as non-booleans
		for (const [n, times] of timesSeen) {
			if (times !== timesSeenCorrectly.get(n)) notBooleanVars.add(n)
		}
	} while (notBooleanVars.size !== initialSize)
	return notBooleanVars
}
const xor = (a: boolean, b: boolean) => +a ^ +b
export function cleanup(block: Block, argTypes: Map<number, string>, localTypes: Map<number, string>, returnType: string): Block {
	const returnBoolean = returnType === BOOLEAN
	const notBooleanVars = getNotBooleanVars(block, argTypes, localTypes, returnBoolean)
	for (const n of localTypes.keys()) {
		if (!notBooleanVars.has(n)) localTypes.set(n, BOOLEAN)
	}
	const strategies: CleanupStrategy[] = [
		...STRATEGIES,
		block => {
			const replacements = new Map<Expression, Expression>()
			//Convert 1 and 0 to true and false, if known to be boolean
			walkBooleanContextExpressions(block, notBooleanVars, returnBoolean, expression => {
				if (expression instanceof IntegerLiteral) {
					replacements.set(expression, new BooleanLiteral(!!expression.i))
				}
			})
			//Simplify ==/!= true/false
			walkBlockExpressions(block, expression => {
				if (expression instanceof BinaryOperation) {
					const {op, arg1, arg2} = expression
					if ((op === '==' || op === '!=') && arg2 instanceof BooleanLiteral) {
						replacements.set(expression,
							xor(op === '==', arg2.b) ? new UnaryOperation('!', arg1) : arg1
						)
					}
				}
			})
			return {
				expressions: replacements,
				statements: new Map
			}
		}
	]
	let someReplaced: boolean
	do {
		someReplaced = false
		for (const strategy of strategies) {
			const replacements = strategy(block)
			if (replacements.expressions.size || replacements.statements.size) {
				block = replaceBlock(block, replacements)
				someReplaced = true
			}
		}
	} while (someReplaced)
	return block
}
export function convertClassString(clazz: string, imports: Set<string>) {
	if (!clazz.includes('/')) return clazz

	const dotString = clazz.replace(/\//g, '.')
	const packages = dotString.split('.')
	if (!(packages.length === 3 && packages[0] === 'java' && packages[1] === 'lang')) {
		imports.add(dotString.replace(/(?:\[\])+$/, ''))
	}
	const [clazzName] = packages.slice(-1)
	return clazzName
}
//Replaces, e.g. java/util/ArrayList with ArrayList
export function resolvePackageClasses(block: Block, imports: Set<string>): Block {
	const replacements = new Map<Expression, Expression>()
	walkBlockExpressions(block, expression => {
		if (expression instanceof ClassLiteral) {
			replacements.set(expression, new ClassLiteral({
				name: convertClassString(expression.clazz.name, imports)
			}))
		}
		else if (expression instanceof ClassReference) {
			replacements.set(expression, new ClassReference({
				name: convertClassString(expression.clazz.name, imports)
			}))
		}
		else if (expression instanceof NewObject) {
			replacements.set(expression, new NewObject(
				{name: convertClassString(expression.clazz.name, imports)},
				expression.args
			))
		}
		else if (expression instanceof NewArray && !expression.primitive) {
			replacements.set(expression, new NewArray(
				{name: convertClassString(expression.type.name, imports)},
				expression.dimensions,
				expression.primitive,
				expression.elements
			))
		}
		else if (expression instanceof Cast && !expression.primitive) {
			replacements.set(expression, new Cast(
				{name: convertClassString(expression.type.name, imports)},
				expression.exp,
				expression.primitive
			))
		}
	})
	return replaceBlock(block, {expressions: replacements, statements: new Map})
}

export function getUsedVariables(block: Block) {
	const variables = new Set<number>()
	walkBlockExpressions(block, expression => {
		if (expression instanceof Variable) variables.add(expression.n)
	})
	return variables
}