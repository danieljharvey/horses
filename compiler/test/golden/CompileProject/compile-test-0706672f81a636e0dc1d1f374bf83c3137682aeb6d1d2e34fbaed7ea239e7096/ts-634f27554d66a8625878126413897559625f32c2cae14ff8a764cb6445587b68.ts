import { Just } from "./ts-40a2cb2053877597240532e98497460451bda893e3616be55d26256cbfe80c79";

/* 
z -> Maybe z
 */
export type Maybe<A> =
  | { type: "Just"; vars: [A] }
  | { type: "Nothing"; vars: [] };
export const Just = <A>(a: A): Maybe<A> => ({ type: "Just", vars: [a] });
export const Nothing: Maybe<never> = { type: "Nothing", vars: [] };
export const main = Just;
