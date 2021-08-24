
import * as egm96 from './egm96'

test('extremes', () => {
  expect(egm96.meanSeaLevel(-90, -180)).not.toBeNull()
  expect(egm96.meanSeaLevel(90, -180)).not.toBeNull()
  expect(egm96.meanSeaLevel(-90, 180)).not.toBeNull()
  expect(egm96.meanSeaLevel(90, 180)).not.toBeNull()
})

test('conversion', () => {
  expect(egm96.egm96ToEllipsoid(50.7129201, 5.6688935, 55.231)).toBeCloseTo(101.698, 3)
  expect(egm96.ellipsoidToEgm96(50.7129201, 5.6688935, 101.698)).toBeCloseTo(55.231, 3)
})
