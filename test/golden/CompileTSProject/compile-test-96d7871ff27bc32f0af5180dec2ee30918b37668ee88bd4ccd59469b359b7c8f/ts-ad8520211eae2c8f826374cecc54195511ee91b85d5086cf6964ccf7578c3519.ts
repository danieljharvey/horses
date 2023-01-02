
import * as Either from "./ts-9fdeed9ad4407124e5c018a227277a27e01b2f27b9a39be94d0fbd36340514ba";




/* 
(e -> o) -> (Either d e) -> Either d o
 */
export const main = <F,P>(f: (arg: F) => P) => <E>(value: Either.Either<E,F>) => { const match = (value: Either.Either<E,F>): Either.Either<E,P> => { if (value.type === `Right`) { const { vars: [a] } = value; return Either.Right(f(a)); }; if (value.type === `Left`) { const { vars: [e] } = value; return Either.Left(e); }; throw new Error("Pattern match error"); }; return match(value); }