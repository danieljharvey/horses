import { __eq, __concat, __patternMatch } from '../../../static/backend/es-modules-js/stdlib.mjs'
const id = a => a
const aRecord = {a:100}
const aPair = [1,2]
export const main = a => b => ({ type: "These", vars: [a,b] });

console.log(main)