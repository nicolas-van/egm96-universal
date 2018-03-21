var fs = require('fs');
var path = require('path');

var data = fs.readFileSync(path.join(__dirname,'/WW15MGH.DAC'));

function toDegree(radians) {
  return radians * (180 / Math.PI);
}

function fromDegree(degrees) {
  return degrees * (Math.PI / 180);
}

function getPostOffset(row, col) {
  var k = row * NUM_COLS + col;

  if(k >= data.length * 2) {
    throw new RangeError('Offset exceeds height measurements');
  }

  return data.readInt16BE(k * 2);
}

var INTERVAL = fromDegree(15/60),
    INTERVAL_DEGREE = toDegree(INTERVAL),
    NUM_ROWS = 721,
    NUM_COLS = 1440;

// http://cddis.gsfc.nasa.gov/926/egm96/egm96.html
function getOffset(latitude, longitude) {
  var longitude = longitude >= 0 ? longitude : longitude + 360;

  var topRow = Math.round((90 - latitude) / INTERVAL_DEGREE);
  if(latitude <= -90) topRow = NUM_ROWS - 2;
  var bottomRow = topRow + 1;

  var leftCol = Math.round(longitude / INTERVAL_DEGREE);
  var rightCol = leftCol + 1;

  if(longitude >= 360 - INTERVAL_DEGREE) {
    leftCol = NUM_COLS - 1;
    rightCol = 0;
  }

  var latTop = 90 - topRow * INTERVAL_DEGREE;
  var lonLeft = leftCol * INTERVAL_DEGREE;

  var ul = getPostOffset(topRow, leftCol);
  var ll = getPostOffset(bottomRow, leftCol);
  var lr = getPostOffset(bottomRow, rightCol);
  var ur = getPostOffset(topRow, rightCol);

  var u = (longitude - lonLeft) / INTERVAL_DEGREE;
  var v = (latTop - latitude) / INTERVAL_DEGREE;

  var pll = (1.0 - u) * (1.0 - v);
  var plr = (1.0 - u) * v;
  var pur = u * v;
  var pul = u * (1.0 - v);

  var offset = pll * ll + plr * lr + pur * ur + pul * ul;

  return offset / 100;
};

console.log(getOffset(38.6281550, 269.7791550));
module.exports = getOffset;
