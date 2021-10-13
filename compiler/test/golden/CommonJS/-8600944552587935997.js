const { __eq, __concat, __patternMatch } = require('../../../static/backend/commonjs/stdlib.js')
const id = a => a
const aRecord = {a:100}
const aPair = [1,2]
const main = function() { const { cat: b, dog: a } = { cat: 2, dog: 1 };
return [a,b] }();

console.log(main)