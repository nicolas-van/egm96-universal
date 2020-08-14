
import { base64 } from 'rfc4648'
import data from './data'

const datab = base64.parse(data)
const dt = new DataView(datab.buffer, datab.byteOffset, datab.byteLength)

const getData = (id) => {
  return dt.getInt16(id * 2, false)
}

const getValue = (row, col) => {
  const k = row * NUM_COLS + col

  return getData(k) / 100
}

const degreesToRadians = (degrees) => {
  return degrees * (Math.PI / 180)
}

const normalizeRadians = (rads, center = 0) => {
  return rads - (2 * Math.PI) * Math.floor((rads + Math.PI - center) / (2 * Math.PI))
}

const INTERVAL = degreesToRadians(15 / 60)
const NUM_ROWS = 721
const NUM_COLS = 1440

/**
 * Gets the mean sea level according to the EGM96.
 *
 * @param {number} latitude The latitude in degrees
 * @param {number} longitude The longitude in degrees
 * @returns {number} The mean sea level in meters, relative to the WGS84 ellipsoid.
 */
export function meanSeaLevel (latitude, longitude) {
  const lat = normalizeRadians(degreesToRadians(latitude))
  const lon = normalizeRadians(degreesToRadians(longitude), Math.PI)

  let topRow = Math.floor(((Math.PI / 2) - lat) / INTERVAL)
  topRow = topRow === NUM_ROWS - 1 ? topRow - 1 : topRow
  const bottomRow = topRow + 1

  const leftCol = Math.floor(lon / INTERVAL)
  const rightCol = (leftCol + 1) % NUM_COLS

  const topLeft = getValue(topRow, leftCol)
  const bottomLeft = getValue(bottomRow, leftCol)
  const bottomRight = getValue(bottomRow, rightCol)
  const topRight = getValue(topRow, rightCol)

  const lonLeft = leftCol * INTERVAL
  const latTop = (Math.PI / 2) - (topRow * INTERVAL)

  const leftProp = (lon - lonLeft) / INTERVAL
  const topProp = (latTop - lat) / INTERVAL

  return bilinearInterpolation(topLeft, bottomLeft, bottomRight, topRight, leftProp, topProp)
}

const bilinearInterpolation = (topLeft, bottomLeft, bottomRight, topRight, x, y) => {
  const top = linearInterpolation(topLeft, topRight, x)
  const bottom = linearInterpolation(bottomLeft, bottomRight, x)

  return linearInterpolation(top, bottom, y)
}

const linearInterpolation = (a, b, prop) => {
  return a + ((b - a) * prop)
}

/**
 * Converts a WGS84's ellipsoid-relative altitude to an EGM96-relative
 * altitude.
 *
 * @param {number} latitude The latitude in degrees
 * @param {number} longitude The longitude in degrees
 * @param {number} altitude The altitude relative to the WGS84's ellipsoid in meters
 * @returns {number} The altitude relative to EGM96 in meters
 */
export function ellipsoidToEgm96 (latitude, longitude, altitude) {
  const ms = meanSeaLevel(latitude, longitude)
  return altitude - ms
}

/**
 * Converts an EGM96-relative altitude to a WGS84's ellipsoid-relative
 * altitude.
 *
 * @param {number} latitude The latitude in degrees
 * @param {number} longitude The longitude in degrees
 * @param {number} altitude The altitude relative to EGM96 in meters
 * @returns {number} The altitude relative to the WGS84's ellipsoid in meters
 */
export function egm96ToEllipsoid (latitude, longitude, altitude) {
  const ms = meanSeaLevel(latitude, longitude)
  return altitude + ms
}
