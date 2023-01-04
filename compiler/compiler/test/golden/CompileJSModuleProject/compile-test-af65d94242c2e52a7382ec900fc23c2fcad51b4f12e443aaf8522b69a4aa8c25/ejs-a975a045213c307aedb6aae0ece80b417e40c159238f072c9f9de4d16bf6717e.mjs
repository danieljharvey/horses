
import * as Either from "./ejs-e4cd0fbcbb6e347c96c4b50f3d3b44e0cc9c78208e33316b058a9ade68416d47.mjs";




/* 
(Either c Boolean) -> Boolean
 */
export const main = (val) => { const match = (value) => { if (value.type === `Right`) { const { vars: [a] } = value; return a; }; if (true) { return false; }; throw new Error("Pattern match error"); }; return match(val); }