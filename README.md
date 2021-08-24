# egm96-universal

[![Node.js CI](https://github.com/nicolas-van/egm96-universal/workflows/Node.js%20CI/badge.svg)](https://github.com/nicolas-van/egm96-universal/actions?query=workflow%3A%22Node.js+CI%22) [![npm](https://img.shields.io/npm/v/egm96-universal)](https://www.npmjs.com/package/egm96-universal) [![Coverage Status](https://coveralls.io/repos/github/nicolas-van/egm96-universal/badge.svg?branch=master)](https://coveralls.io/github/nicolas-van/egm96-universal?branch=master)

A library to convert between EGM96-relative altitudes and WGS84 ellipsoid-relative altitudes.

Works both in the browser and node.js, you can test the result on [the demo page](https://nicolas-van.github.io/egm96-universal/).

It uses the EGM96 sample data file provided by the [NGA](https://en.wikipedia.org/wiki/National_Geospatial-Intelligence_Agency) to lookup reference mean sea level and performs bilinear interpolation on the result.

This library is automatically tested by CI on [the reference implementation in Fortran](https://earth-info.nga.mil/GandG/wgs84/gravitymod/egm96/binary/binarygeoid.html).

This project was initially created to allow usage of KML files (that use EGM96 as reference according to the [specification](https://www.ogc.org/standards/kml), just like Google Earth) in [Cesium](https://cesium.com/index.html). It should work for any similar task that necessitate conversion between the two references.

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
import * as egm96 from 'egm96-universal'
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

## Changelog

### 1.1.0

* Added Typescript definitions

## License

[See the license file](./LICENSE.md).

## Contributing

[See the contribution guide](./CONTRIBUTING.md).
