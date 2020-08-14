# egm96-universal

![Node.js CI](https://github.com/nicolas-van/egm96-universal/workflows/Node.js%20CI/badge.svg) [![npm](https://img.shields.io/npm/v/egm96-universal)](https://www.npmjs.com/package/egm96-universal)

A library to convert between EGM96-relative altitudes and WGS84 ellipsoid-relative altitudes.

It uses the EGM96 sample data file provided by the [NGA](https://en.wikipedia.org/wiki/National_Geospatial-Intelligence_Agency) to lookup reference mean sea level and performs bilinear interpolation on the result.

This library is automatically tested by CI on [the reference implementation in Fortran](https://earth-info.nga.mil/GandG/wgs84/gravitymod/egm96/binary/binarygeoid.html).

Works both in the browser and node.js.

## Usage

### Installation

```
npm install egm96-universal
```

### Import

```
const egm96 = require('egm96-universal')
```

or

```
import egm96 from 'egm96-universal'
```

### Getting mean sea level according to EGM96

```
const msl = egm96.meanSeaLevel(latitudeInDegrees, longitudeInDegrees)
// mean sea level in meters relative to the WGS84 ellipsoid
```

### Convert between WGS84 ellipsoid-relative altitude and EGM96-relative altitude

```
const egm96Alt = egm96.ellipsoidToEgm96(latitude, longitude, altitude)
```

```
const ellipsoidAlt = egm96.egm96ToEllipsoid(latitude, longitude, altitude)
```

## License

[See the license file](./LICENSE.md).

## Contributing

[See the contribution guide](./CONTRIBUTING.md).
