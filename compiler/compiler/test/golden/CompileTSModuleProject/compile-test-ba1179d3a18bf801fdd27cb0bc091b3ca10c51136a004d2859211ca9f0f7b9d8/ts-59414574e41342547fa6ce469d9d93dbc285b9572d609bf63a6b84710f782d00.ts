
export type Identity<A> = { type: "Identity", vars: [A] }; export const Identity = <A>(a: A): Identity<A> => ({ type: "Identity", vars: [a] }); 