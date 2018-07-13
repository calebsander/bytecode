import {Parser, parseRepeated} from './parse'
import {constantParser} from './constant-parser'
import ConstantPoolIndex from './constant-pool-index'
import {Class} from './constant-pool-parser'

export const interfacesParser: Parser<ConstantPoolIndex<Class>[]> =
	parseRepeated(constantParser)