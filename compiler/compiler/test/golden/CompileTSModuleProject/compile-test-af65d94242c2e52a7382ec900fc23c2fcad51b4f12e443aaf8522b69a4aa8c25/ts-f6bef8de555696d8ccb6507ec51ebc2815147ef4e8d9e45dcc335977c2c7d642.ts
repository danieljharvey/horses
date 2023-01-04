import { main as useEither } from "./ts-a975a045213c307aedb6aae0ece80b417e40c159238f072c9f9de4d16bf6717e";



import type { Either } from "./ts-e4cd0fbcbb6e347c96c4b50f3d3b44e0cc9c78208e33316b058a9ade68416d47";


/* 
(Either c Boolean) -> Boolean
 */
export const main = <D>(val: Either<D,boolean>) => useEither(val)