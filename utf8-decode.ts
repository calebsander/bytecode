const MAX_ARGUMENTS_LENGTH = 0x1000
export default (buffer: Uint8Array): string => {
	//Taken from https://github.com/feross/buffer/blob/da8a677bdb746ed9d6dae42ee1eaf236aad32ccb/index.js#L917-L988
	const codePoints = []
	for (let i = 0; i < buffer.length;) {
		const firstByte = buffer[i]
		let codePoint
		let bytesPerSequence: 1 | 2 | 3 | 4
			= firstByte > 0xEF ? 4
			: firstByte > 0xDF ? 3
			: firstByte > 0xBF ? 2
			: 1

		if (i + bytesPerSequence <= buffer.length) {
			let secondByte: number,
				thirdByte: number,
				fourthByte: number,
				tempCodePoint: number
			//tslint:disable-next-line:switch-default
			switch (bytesPerSequence) {
				case 1:
					if (firstByte < 0x80) codePoint = firstByte
					break
				case 2:
					secondByte = buffer[i + 1]
					if ((secondByte & 0xC0) === 0x80) {
						tempCodePoint = (firstByte & 0x1F) << 0x6 | (secondByte & 0x3F)
						if (tempCodePoint > 0x7F) codePoint = tempCodePoint
					}
					break
				case 3:
					secondByte = buffer[i + 1]
					thirdByte = buffer[i + 2]
					if ((secondByte & 0xC0) === 0x80 && (thirdByte & 0xC0) === 0x80) {
						tempCodePoint = (firstByte & 0xF) << 0xC | (secondByte & 0x3F) << 0x6 | (thirdByte & 0x3F)
						if (tempCodePoint > 0x7FF && (tempCodePoint < 0xD800 || tempCodePoint > 0xDFFF)) codePoint = tempCodePoint
					}
					break
				/*istanbul ignore next*/
				case 4:
					secondByte = buffer[i + 1]
					thirdByte = buffer[i + 2]
					fourthByte = buffer[i + 3]
					if ((secondByte & 0xC0) === 0x80 && (thirdByte & 0xC0) === 0x80 && (fourthByte & 0xC0) === 0x80) {
						tempCodePoint = (firstByte & 0xF) << 0x12 | (secondByte & 0x3F) << 0xC | (thirdByte & 0x3F) << 0x6 | (fourthByte & 0x3F)
						if (tempCodePoint > 0xFFFF && tempCodePoint < 0x110000) codePoint = tempCodePoint
					}
			}
		}
		if (codePoint === undefined) {
			codePoint = 0xFFFD
			bytesPerSequence = 1
		}
		else {
			/*istanbul ignore if*/
			if (codePoint > 0xFFFF) {
				codePoint -= 0x10000
				codePoints.push(codePoint >>> 10 & 0x3FF | 0xD800)
				codePoint = 0xDC00 | codePoint & 0x3FF
			}
		}
		codePoints.push(codePoint)
		i += bytesPerSequence
	}
	let str = ''
	for (let i = 0; i < codePoints.length; i += MAX_ARGUMENTS_LENGTH) {
		str += String.fromCharCode(...codePoints.slice(i, i + MAX_ARGUMENTS_LENGTH))
	}
	return str
}