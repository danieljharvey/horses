
import * as Maybe from "./ejs-f27c54e32f5feb63689584a7a4ffa5cb7d9f922682d0395f93d76f791e16951a.mjs";




/* 
(a -> i) -> (Maybe a) -> Maybe i
 */
export const main = (f) => (maybeA) => { const match = (value) => { if (value.type === `Just`) { const { vars: [a] } = value; return Maybe.Just(f(a)); }; if (true) { return Maybe.Nothing; }; throw new Error("Pattern match error"); }; return match(maybeA); }