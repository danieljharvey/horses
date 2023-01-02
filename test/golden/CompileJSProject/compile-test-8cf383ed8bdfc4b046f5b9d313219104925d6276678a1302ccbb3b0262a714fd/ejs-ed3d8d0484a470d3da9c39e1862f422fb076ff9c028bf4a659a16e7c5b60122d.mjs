
import * as Maybe from "./ejs-3c85e4089872d4848160202abed8276aea6c2c539403bb34f3648b79a33fbd80.mjs";




/* 
(a -> i) -> (Maybe a) -> Maybe i
 */
export const main = (f) => (maybeA) => { const match = (value) => { if (value.type === `Just`) { const { vars: [a] } = value; return Maybe.Just(f(a)); }; if (true) { return Maybe.Nothing; }; throw new Error("Pattern match error"); }; return match(maybeA); }