
import * as Either from "./ts-9fdeed9ad4407124e5c018a227277a27e01b2f27b9a39be94d0fbd36340514ba";




/* 
(Either c Boolean) -> Boolean
 */
export const main = <D>(val: Either.Either<D,boolean>) => { const match = (value: Either.Either<D,boolean>): boolean => { if (value.type === `Right`) { const { vars: [a] } = value; return a; }; if (true) { return false; }; throw new Error("Pattern match error"); }; return match(val); }