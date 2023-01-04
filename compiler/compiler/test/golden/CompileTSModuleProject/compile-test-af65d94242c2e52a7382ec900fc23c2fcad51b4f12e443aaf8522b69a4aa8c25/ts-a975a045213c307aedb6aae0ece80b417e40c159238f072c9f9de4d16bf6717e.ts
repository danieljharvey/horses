
import * as Either from "./ts-e4cd0fbcbb6e347c96c4b50f3d3b44e0cc9c78208e33316b058a9ade68416d47";




/* 
(Either c Boolean) -> Boolean
 */
export const main = <D>(val: Either.Either<D,boolean>) => { const match = (value: Either.Either<D,boolean>): boolean => { if (value.type === `Right`) { const { vars: [a] } = value; return a; }; if (true) { return false; }; throw new Error("Pattern match error"); }; return match(val); }