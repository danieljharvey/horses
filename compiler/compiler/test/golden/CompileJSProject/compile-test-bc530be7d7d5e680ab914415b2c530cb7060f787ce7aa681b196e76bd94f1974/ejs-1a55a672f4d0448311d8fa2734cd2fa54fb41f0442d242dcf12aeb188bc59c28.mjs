
import * as Either from "./ejs-e4cd0fbcbb6e347c96c4b50f3d3b44e0cc9c78208e33316b058a9ade68416d47.mjs";




/* 
(e -> o) -> (Either d e) -> Either d o
 */
export const main = (f) => (value) => { const match = (value) => { if (value.type === `Right`) { const { vars: [a] } = value; return Either.Right(f(a)); }; if (value.type === `Left`) { const { vars: [e] } = value; return Either.Left(e); }; throw new Error("Pattern match error"); }; return match(value); }