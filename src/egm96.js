
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

  const ul = gePostOffset(topRow, leftCol)
  const ll = gePostOffset(bottomRow, leftCol)
  const lr = gePostOffset(bottomRow, rightCol)
  const ur = gePostOffset(topRow, rightCol)

  const latBottom = 90 - (bottomRow * INTERVAL)
  const latTop = 90 - (topRow * INTERVAL)
  const lonLeft = leftCol * INTERVAL
  const lonRight = rightCol * INTERVAL

  const leftDistance = (lon - lonLeft) / INTERVAL
  const bottomDistance = (lat - latBottom) / INTERVAL
  const rightDistance = (lonRight - lon) / INTERVAL
  const topDistance = (latTop - lat) / INTERVAL

  const pll = rightDistance * topDistance
  const plr = leftDistance * topDistance
  const pur = leftDistance * bottomDistance
  const pul = rightDistance * bottomDistance

  const tot = pll + plr + pur + pul

  const offset = (pll * ll) / tot + (plr * lr) / tot + (pur * ur) / tot + (pul * ul) / tot

  return offset
}
