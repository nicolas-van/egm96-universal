
import { base64 } from 'rfc4648'
import data from './data'

const datab = base64.parse(data)
const dt = new DataView(datab.buffer, datab.byteOffset, datab.byteLength)

/**
 * Get the mean sea level of specified coordinates depending of spots on geoid using the Earth
 * Gravity Model 1996.
 *
 * @param {number} lat The latitude of the position
 * @param {number} lon The longitude of the position
 * @returns {number} mean sea level of specified coordinates relative to WGS84's ellipsoid
 */
export function getGeoidMeanSeaLevel (lat, lon) {
  // in a first time find on a grid of 721 lines(lat) and 1440 columns(lon)
  const recordindex = Math.floor((lat - 90) * -1 / 0.25)
  if (lon < 0) {
    lon += 360
  }
  const heightindex = Math.floor(lon / 0.25)
  const idxUpperLeft = (1440 * recordindex) + heightindex
  const idxUpperRight = (1440 * recordindex) + ((heightindex + 1) % 1440)
  let idxBottomLeft
  let idxBottomRight
  if (recordindex < 721) {
    idxBottomLeft = (1440 * (recordindex + 1)) + heightindex
    idxBottomRight = (1440 * (recordindex + 1)) + ((heightindex + 1) % 1440)
  } else {
    idxBottomLeft = idxUpperLeft
    idxBottomRight = idxUpperRight
  }
  const weightUL = 1 / distance(lat, lon, getLat(idxUpperLeft), getLon(idxUpperLeft))
  const weightUR = 1 / distance(lat, lon, getLat(idxUpperRight), getLon(idxUpperRight))
  const weightBL = 1 / distance(lat, lon, getLat(idxBottomLeft), getLon(idxBottomLeft))
  const weightBR = 1 / distance(lat, lon, getLat(idxBottomRight), getLon(idxBottomRight))

  return ((getData(idxUpperLeft) * weightUL +
                          getData(idxUpperRight) * weightUR +
                          getData(idxBottomLeft) * weightBL +
                          getData(idxBottomRight) * weightBR) /
                  (weightUL + weightUR + weightBL + weightBR)) /
          100
}

const getData = (id) => {
  return dt.getInt16(id * 2, false)
}

const getLat = (idx) => {
  return ((idx - idx % 1440) / 1440) * 0.25
}

const getLon = (idx) => {
  return (idx % 1440) * 0.25
}

const distance = (lat1, lon1, lat2, lon2) => {
  return Math.sqrt(Math.pow((lat2 - lat1), 2) + Math.pow((lon2 - lon1), 2))
}
