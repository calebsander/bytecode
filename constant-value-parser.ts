import {parseAndThen, parseReturn} from './parse'
import {constantParser} from './constant-parser'
import {ConstantPool} from './constant-pool-parser'

export const constantValueParser = (constantPool: ConstantPool) =>
	parseAndThen(constantParser, constant =>
		parseReturn(constant.getValue(constantPool))
	)