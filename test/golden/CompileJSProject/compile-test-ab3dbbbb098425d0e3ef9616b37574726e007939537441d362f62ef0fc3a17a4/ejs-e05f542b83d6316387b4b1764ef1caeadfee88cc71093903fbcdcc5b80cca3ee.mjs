
import * as Maybe from "./ejs-3c85e4089872d4848160202abed8276aea6c2c539403bb34f3648b79a33fbd80.mjs";




/* 
(Maybe a) -> a -> a
 */
export const main = (val) => (fallback) => { const match = (value) => { if (value.type === `Just`) { const { vars: [a] } = value; return a; }; if (true) { return fallback; }; throw new Error("Pattern match error"); }; return match(val); }