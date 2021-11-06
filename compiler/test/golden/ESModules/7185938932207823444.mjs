import { __eq, __concat, __patternMatch } from '../../../static/backend/es-modules-js/stdlib.mjs'
const id = a => a
const aRecord = {a:100}
const aPair = [1,2]
export const main = function() { const { vars: [a] } = { type: "Ident", vars: [1] };
return a }();

console.log(main)