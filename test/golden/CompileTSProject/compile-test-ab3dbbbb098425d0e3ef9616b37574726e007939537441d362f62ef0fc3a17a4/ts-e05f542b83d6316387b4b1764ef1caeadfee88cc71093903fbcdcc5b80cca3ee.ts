
import * as Maybe from "./ts-3c85e4089872d4848160202abed8276aea6c2c539403bb34f3648b79a33fbd80";




/* 
(Maybe a) -> a -> a
 */
export const main = <B>(val: Maybe.Maybe<B>) => (fallback: B) => { const match = (value: Maybe.Maybe<B>): B => { if (value.type === `Just`) { const { vars: [a] } = value; return a; }; if (true) { return fallback; }; throw new Error("Pattern match error"); }; return match(val); }