
import * as egm96 from './egm96'

test('base test', async () => {
  expect(egm96.getGeoidMeanSeaLevel(0, 0)).toBeCloseTo(17.144, 2)
  expect(egm96.getGeoidMeanSeaLevel(10, 10)).toBeCloseTo(21.062, 2)
})
