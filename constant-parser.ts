import {Parser, parseAndThen, parseReturn, parseShort} from './parse'
import ConstantPoolIndex from './constant-pool-index'

export const constantParser: Parser<ConstantPoolIndex<any>> =
	parseAndThen(parseShort, index =>
		parseReturn(new ConstantPoolIndex(index))
	)