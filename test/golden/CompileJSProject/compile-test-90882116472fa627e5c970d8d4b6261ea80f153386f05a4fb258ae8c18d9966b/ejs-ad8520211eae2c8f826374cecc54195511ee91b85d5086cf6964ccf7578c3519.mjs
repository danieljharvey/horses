
import * as Either from "./ejs-9fdeed9ad4407124e5c018a227277a27e01b2f27b9a39be94d0fbd36340514ba.mjs";




/* 
(e -> o) -> (Either d e) -> Either d o
 */
export const main = (f) => (value) => { const match = (value) => { if (value.type === `Right`) { const { vars: [a] } = value; return Either.Right(f(a)); }; if (value.type === `Left`) { const { vars: [e] } = value; return Either.Left(e); }; throw new Error("Pattern match error"); }; return match(value); }