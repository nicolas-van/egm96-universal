
import * as egm96 from './egm96'

test('base test', async () => {
  expect(egm96.getGeoidMeanSeaLevel(0, 0)).toBeCloseTo(17.144, 2)
  expect(egm96.getGeoidMeanSeaLevel(10, 10)).toBeCloseTo(21.062, 2)
  expect(egm96.getGeoidMeanSeaLevel(50.7129201, 5.6688935)).toBeCloseTo(46.678, 2)
  expect(egm96.getGeoidMeanSeaLevel(50.7113365, 5.6797431)).toBeCloseTo(46.678, 2)
})
