//
// luxafor-set.js
//
// Send luxafor to a specific color
//
// Usage:
// node luxafor-set.js ''
//


const Luxafor = require('luxafor-api');

// Connect to Luxafor light
const device = new Luxafor();

// Set the text to send
const args = process.argv.slice(2)
if (args[0] && args[0].length) {
  color = args[0];
}

console.log(args);
if (color) {
  device.setColor('#'+color, 0xFF);
} else {
  device.off();
}
