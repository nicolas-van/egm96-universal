
import { base64 } from 'rfc4648'
import data from './data'

const datab = base64.parse(data)
const dt = new DataView(datab.buffer, datab.byteOffset, datab.byteLength)

const getData = (id) => {
  return dt.getInt16(id * 2, false)
}

const gePostOffset = (row, col) => {
  const k = row * NUM_COLS + col

  return getData(k) / 100
}

const degreesToRadians = (degrees) => {
  return degrees * (Math.PI / 180);
}

const normalizeRadians = (rads, center = 0) => {
  return rads - (2 * Math.PI) * Math.floor((rads + Math.PI - center) / (2 * Math.PI))
}

const INTERVAL = degreesToRadians(15 / 60)
const NUM_ROWS = 721
const NUM_COLS = 1440

/**
 * @override
 */
export function getGeoidMeanSeaLevel (latitude, longitude) {
  const lat = normalizeRadians(degreesToRadians(latitude))
  const lon = normalizeRadians(degreesToRadians(longitude), Math.PI)

  let topRow = Math.floor(((Math.PI / 2) - lat) / INTERVAL)
  topRow = topRow === NUM_ROWS - 1 ? topRow - 1 : topRow
  const bottomRow = topRow + 1

  const leftCol = Math.floor(lon / INTERVAL)
  const rightCol = (leftCol + 1) % NUM_COLS

  const ul = gePostOffset(topRow, leftCol)
  const ll = gePostOffset(bottomRow, leftCol)
  const lr = gePostOffset(bottomRow, rightCol)
  const ur = gePostOffset(topRow, rightCol)

  const lonLeft = leftCol * INTERVAL
  const latTop = (Math.PI / 2) - (topRow * INTERVAL)

  const leftProp = (lon - lonLeft) / INTERVAL
  const topProp = (latTop - lat) / INTERVAL

  return bilinearInterpolation(ul, ll, lr, ur, leftProp, topProp)
}

const bilinearInterpolation = (ul, ll, lr, ur, x, y) => {
  const top = linearInterpolation(ul, ur, x)
  const bottom = linearInterpolation(ll, lr, x)

  return linearInterpolation(top, bottom, y)
}

const linearInterpolation = (a, b, prop) => {
  return a + ((b - a) * prop)
}
