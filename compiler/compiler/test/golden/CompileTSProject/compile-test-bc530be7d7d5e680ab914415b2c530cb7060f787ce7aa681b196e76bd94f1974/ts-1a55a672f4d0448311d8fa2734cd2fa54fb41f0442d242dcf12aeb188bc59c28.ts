
import * as Either from "./ts-e4cd0fbcbb6e347c96c4b50f3d3b44e0cc9c78208e33316b058a9ade68416d47";




/* 
(e -> o) -> (Either d e) -> Either d o
 */
export const main = <F,P>(f: (arg: F) => P) => <E>(value: Either.Either<E,F>) => { const match = (value: Either.Either<E,F>): Either.Either<E,P> => { if (value.type === `Right`) { const { vars: [a] } = value; return Either.Right(f(a)); }; if (value.type === `Left`) { const { vars: [e] } = value; return Either.Left(e); }; throw new Error("Pattern match error"); }; return match(value); }