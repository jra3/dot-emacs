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
    cmd = args[0];
}

if (cmd == "off") {
  console.log("off");
  device.off();
} else if (cmd == "set") {
  console.log("set");
  device.setColor(args[1], 0xFF);
} else if (cmd == "flash") {
  console.log("flash");
  device.flash(args[1], 0xFF, 40, 1);
} else {

}
