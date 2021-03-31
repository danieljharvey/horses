const { __match, __eq, __concat } = require("./cjs-stdlib.js");
const main = a => ({ type: "Task", vars: [r => r(a)] });
module.exports = { main: main }