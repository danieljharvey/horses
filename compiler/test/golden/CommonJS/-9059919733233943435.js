const { __eq, __concat, __patternMatch } = require('../../../static/backend/commonjs/stdlib.js')
const id = a => a
const aRecord = {a:100}
const aPair = [1,2]
const main = function() { const { vars: [a] } = { type: "Ident", vars: [1] };
return a }();

console.log(main)