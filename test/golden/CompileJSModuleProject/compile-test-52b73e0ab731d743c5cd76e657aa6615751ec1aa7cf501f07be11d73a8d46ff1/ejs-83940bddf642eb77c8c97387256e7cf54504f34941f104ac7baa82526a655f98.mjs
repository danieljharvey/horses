
import * as Either from "./ejs-9fdeed9ad4407124e5c018a227277a27e01b2f27b9a39be94d0fbd36340514ba.mjs";




/* 
(Either c Boolean) -> Boolean
 */
export const main = (val) => { const match = (value) => { if (value.type === `Right`) { const { vars: [a] } = value; return a; }; if (true) { return false; }; throw new Error("Pattern match error"); }; return match(val); }