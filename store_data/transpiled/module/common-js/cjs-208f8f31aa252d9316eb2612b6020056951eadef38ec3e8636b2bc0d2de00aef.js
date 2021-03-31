const { __match, __eq, __concat } = require("./cjs-stdlib.js");
const main = url => __eq(url, "pug") ? { type: "Fetch", vars: ["https://dog.ceo/api/breed/pug/images/random",ret => ({ type: "Respond", vars: [ret] })] } : { type: "Respond", vars: ["Oh no"] };
module.exports = { main: main }