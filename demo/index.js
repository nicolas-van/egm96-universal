
import * as egm96 from '../src/egm96'

document.querySelector('#msl').addEventListener('submit', (e) => {
  e.preventDefault()
  const lat = parseFloat(document.querySelector('#msl-latitude').value)
  const lon = parseFloat(document.querySelector('#msl-longitude').value)
  const msl = egm96.meanSeaLevel(lat, lon)
  const result = document.querySelector('#msl-result')
  result.textContent = `Mean Sea Level: ${msl}m`
  result.style.display = 'inherit'
})

document.querySelector('#eltoeg').addEventListener('submit', (e) => {
  e.preventDefault()
  const lat = parseFloat(document.querySelector('#eltoeg-latitude').value)
  const lon = parseFloat(document.querySelector('#eltoeg-longitude').value)
  const alt = parseFloat(document.querySelector('#eltoeg-altitude').value)
  const val = egm96.ellipsoidToEgm96(lat, lon, alt)
  const result = document.querySelector('#eltoeg-result')
  result.textContent = `EGM96 altitude: ${val}m`
  result.style.display = 'inherit'
})

document.querySelector('#egtoel').addEventListener('submit', (e) => {
  e.preventDefault()
  const lat = parseFloat(document.querySelector('#egtoel-latitude').value)
  const lon = parseFloat(document.querySelector('#egtoel-longitude').value)
  const alt = parseFloat(document.querySelector('#egtoel-altitude').value)
  const val = egm96.egm96ToEllipsoid(lat, lon, alt)
  const result = document.querySelector('#egtoel-result')
  result.textContent = `Ellipsoid altitude: ${val}m`
  result.style.display = 'inherit'
})

document.querySelector('#root').style.display = 'inherit'
