import { __eq, __concat, __patternMatch } from '../../../static/backend/es-modules-js/stdlib.mjs'
const id = a => a
const aRecord = {a:100}
const aPair = [1,2]
export const main = function() { const { cat: b, dog: a } = { cat: 2, dog: 1 };
return [a,b] }();

console.log(main)