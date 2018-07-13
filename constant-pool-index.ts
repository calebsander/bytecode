import {ConstantPool} from './constant-pool-parser'

export default class ConstantPoolIndex<E> {
	constructor(private readonly index: number) {}

	public getValue(constantPool: ConstantPool): E {
		return constantPool.getConstant(this.index)
	}
}