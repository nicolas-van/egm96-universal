const fs = require('fs')
const path = require('path')
const base64 = require('rfc4648').base64

const data = fs.readFileSync(path.join(__dirname, './WW15MGH.DAC'))

const str = base64.stringify(data)

fs.writeFileSync(path.join(__dirname, './src/data.js'), `
/* eslint-disable */
export default ${JSON.stringify(str)}
`)
