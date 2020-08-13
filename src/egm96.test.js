
import * as egm96 from './egm96'
import * as cp from 'child_process'
import path from 'path'
import fs from 'fs'

const clean = (path) => {
  if (fs.existsSync(path)) {
    fs.unlinkSync(path)
  }
}

const getReferenceHeight = (lat, lon) => {
  try {
    clean(path.join(__dirname, '../INPUT.DAT'))
    clean(path.join(__dirname, '../OUTINTPT.DAT'))
    fs.writeFileSync(path.join(__dirname, '../INPUT.DAT'), `${lat} ${lon}`)
    cp.execFileSync(path.join(__dirname, '../reference/intptdac'), {
      cwd: path.join(__dirname, '../'),
      stdio: 'ignore'
    })
    const content = fs.readFileSync(path.join(__dirname, '../OUTINTPT.DAT'), {
      encoding: 'utf-8'
    })
    const r = /\s*([+-]?(?:[0-9]*[.])?[0-9]+)\s+([+-]?(?:[0-9]*[.])?[0-9]+)\s+([+-]?(?:[0-9]*[.])?[0-9]+)\s*/
    const m = r.exec(content)
    console.log(m)
    if (!m[3]) {
      throw new Error('Invalid content')
    }
    return parseFloat(m[3])
  } finally {
    clean(path.join(__dirname, '../INPUT.DAT'))
    clean(path.join(__dirname, '../OUTINTPT.DAT'))
  }
}

test('base test', async () => {
  const testComparison = (lat, lon) => {
    const ref = getReferenceHeight(lat, lon)
    expect(egm96.getGeoidMeanSeaLevel(lat, lon)).toBeCloseTo(ref, 2)
  }
  testComparison(0, 0)
  testComparison(10, 10)
  testComparison(50.7129201, 5.6688935)
  testComparison(50.7113365, 5.6797431)
})
