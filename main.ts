#!/usr/bin/env node

import * as fs from 'fs'
import {promisify} from 'util'
import {classFileParser} from './class-file-parser'
import {classToString} from './decompile-class'

if (process.argv.length !== 3) throw new Error('Needs filename')

promisify(fs.readFile)(process.argv[2])
	.then(({buffer, byteOffset, byteLength}) => {
		const dataView = new DataView(buffer, byteOffset, byteLength)
		const classFile = classFileParser(dataView).result
		console.log(classToString(classFile))
	})