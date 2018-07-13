#!/usr/bin/env node
import * as fs from 'fs'
import {promisify} from 'util'
import {classFileParser} from './class-file-parser'

if (process.argv.length !== 3) throw new Error('Needs filename')

promisify(fs.readFile)(process.argv[2])
	.then(data => {
		classFileParser(new DataView(data.buffer, data.byteOffset, data.byteLength))
	})