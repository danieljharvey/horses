
import * as Maybe from "./ejs-f27c54e32f5feb63689584a7a4ffa5cb7d9f922682d0395f93d76f791e16951a.mjs";




/* 
(Maybe a) -> a -> a
 */
export const main = (val) => (fallback) => { const match = (value) => { if (value.type === `Just`) { const { vars: [a] } = value; return a; }; if (true) { return fallback; }; throw new Error("Pattern match error"); }; return match(val); }