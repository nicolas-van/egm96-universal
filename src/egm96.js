
import { base64 } from 'rfc4648'
import data from './data'

const datab = base64.parse(data)
const dt = new DataView(datab.buffer, datab.byteOffset, datab.byteLength)

const getData = (id) => {
  return dt.getInt16(id * 2, false)
}

const INTERVAL = 15 / 60
const NUM_ROWS = 721
const NUM_COLS = 1440

/**
 * @override
 */
export function getGeoidMeanSeaLevel (latitude, longitude) {
  const lat = latitude
  const lon = longitude >= 0 ? longitude : longitude + 360

  let topRow = Math.floor((90 - lat) / INTERVAL)
  if (lat <= -90) {
    topRow = NUM_ROWS - 2
  }
  const bottomRow = topRow + 1
  let leftCol = Math.floor(lon / INTERVAL)
  let rightCol = leftCol + 1

  if (lon >= 360 - INTERVAL) {
    leftCol = NUM_COLS - 1
    rightCol = 0
  }

  const latBottom = 90 - bottomRow * INTERVAL
  const lonLeft = leftCol * INTERVAL

  const ul = gePostOffset(topRow, leftCol)
  const ll = gePostOffset(bottomRow, leftCol)
  const lr = gePostOffset(bottomRow, rightCol)
  const ur = gePostOffset(topRow, rightCol)

  const u = (lon - lonLeft) / INTERVAL
  const v = (lat - latBottom) / INTERVAL

  const pll = (1.0 - u) * (1.0 - v)
  const plr = u * (1.0 - v)
  const pur = u * v
  const pul = (1.0 - u) * v

  const offset = pll * ll + plr * lr + pur * ur + pul * ul

  return offset / 100
}

const gePostOffset = (row, col) => {
  const k = row * NUM_COLS + col

  return getData(k)
}
