
import { base64 } from 'rfc4648'
import data from './data'

const datab = base64.parse(data)
const dt = new DataView(datab.buffer, datab.byteOffset, datab.byteLength)

const getData = (id) => {
  return dt.getInt16(id * 2, false)
}

const toDegree = (radians) => {
  return radians * (180 / Math.PI)
}

const fromDegree = (degrees) => {
  return degrees * (Math.PI / 180)
}

const getPostOffset = (row, col) => {
  const k = row * NUM_COLS + col

  if (k >= data.length * 2) {
    throw new RangeError('Offset exceeds height measurements')
  }

  return getData(k)
}

const INTERVAL = fromDegree(15 / 60)
const INTERVAL_DEGREE = toDegree(INTERVAL)
const NUM_ROWS = 721
const NUM_COLS = 1440

/**
 * @override
 */
export function getGeoidMeanSeaLevel (latitude, longitude) {
  longitude = longitude >= 0 ? longitude : longitude + 360

  let topRow = Math.round((90 - latitude) / INTERVAL_DEGREE)
  if (latitude <= -90) topRow = NUM_ROWS - 2
  const bottomRow = topRow + 1

  let leftCol = Math.round(longitude / INTERVAL_DEGREE)
  let rightCol = leftCol + 1

  if (longitude >= 360 - INTERVAL_DEGREE) {
    leftCol = NUM_COLS - 1
    rightCol = 0
  }

  const latTop = 90 - topRow * INTERVAL_DEGREE
  const lonLeft = leftCol * INTERVAL_DEGREE

  const ul = getPostOffset(topRow, leftCol)
  const ll = getPostOffset(bottomRow, leftCol)
  const lr = getPostOffset(bottomRow, rightCol)
  const ur = getPostOffset(topRow, rightCol)

  const u = (longitude - lonLeft) / INTERVAL_DEGREE
  const v = (latTop - latitude) / INTERVAL_DEGREE

  const pll = (1.0 - u) * (1.0 - v)
  const plr = (1.0 - u) * v
  const pur = u * v
  const pul = u * (1.0 - v)

  const offset = pll * ll + plr * lr + pur * ur + pul * ul

  return offset / 100
};
