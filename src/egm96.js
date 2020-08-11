
const data = new Uint8Array()

/**
 * @override
 */
function toDegree (radians) {
  return radians * (180 / Math.PI)
}

/**
 * @override
 */
function fromDegree (degrees) {
  return degrees * (Math.PI / 180)
}

/**
 * @override
 */
function getPostOffset (row, col) {
  const k = row * NUM_COLS + col

  if (k >= data.length * 2) {
    throw new RangeError('Offset exceeds height measurements')
  }

  return data.readInt16BE(k * 2)
}

const INTERVAL = fromDegree(15 / 60)
const INTERVAL_DEGREE = toDegree(INTERVAL)
const NUM_ROWS = 721
const NUM_COLS = 1440

// http://cddis.gsfc.nasa.gov/926/egm96/egm96.html
/**
 * Gets the mean sea level at given coordinates relative to WGS84's ellipsoid.
 *
 * @param {number} latitude The latitude in degrees.
 * @param {number} longitude The longitude in degrees.
 * @returns {number} The mean sea level in meters, relative to WGS84's ellipsoid.
 */
export function getMeanSeaLevel (latitude, longitude) {
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
