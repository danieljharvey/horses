const { __match, __eq, __concat } = require("./cjs-stdlib.js");
const main = url => cb => __eq(url, "pug") ? { type: "Fetch", vars: ["https://dog.ceo/api/breed/pug/images/random",ret => ({ type: "Respond", vars: [ret,cb] })] } : { type: "Respond", vars: ["Oh no",cb] };
module.exports = { main: main }