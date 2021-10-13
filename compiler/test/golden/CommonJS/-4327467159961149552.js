const { __eq, __concat, __patternMatch } = require('../../../static/backend/commonjs/stdlib.js')
const id = a => a
const aRecord = {a:100}
const aPair = [1,2]
const main = __patternMatch({ type: "Just", vars: [true] }, [ [ pat => __eq(pat.type, "Just") ? { a: pat.vars[0] } : null, ({ a }) => { const b = 1;
return { type: "Just", vars: [a] } } ], [ pat => ({}), () => ({ type: "Nothing", vars: [] }) ] ]);

console.log(main)