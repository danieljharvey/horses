import { __eq, __concat, __patternMatch } from '../../../static/backend/es-modules-js/stdlib.mjs'
const id = a => a
const aRecord = {a:100}
const aPair = [1,2]
export const main = __patternMatch({ type: "Just", vars: [true] }, [ [ pat => __eq(pat.type, "Just") ? { a: pat.vars[0] } : null, ({ a }) => ({ type: "Just", vars: [a] }) ], [ pat => ({}), () => ({ type: "Nothing", vars: [] }) ] ]);

console.log(main)