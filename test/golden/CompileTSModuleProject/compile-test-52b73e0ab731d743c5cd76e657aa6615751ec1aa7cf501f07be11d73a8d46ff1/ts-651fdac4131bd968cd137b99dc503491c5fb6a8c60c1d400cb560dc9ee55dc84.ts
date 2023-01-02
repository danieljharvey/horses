import { main as useEither } from "./ts-83940bddf642eb77c8c97387256e7cf54504f34941f104ac7baa82526a655f98";



import type { Either } from "./ts-9fdeed9ad4407124e5c018a227277a27e01b2f27b9a39be94d0fbd36340514ba";


/* 
(Either c Boolean) -> Boolean
 */
export const main = <D>(val: Either<D,boolean>) => useEither(val)