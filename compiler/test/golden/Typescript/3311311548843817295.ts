type Maybe<A> = { type: "Just", vars: [A] } | { type: "Nothing", vars: [] }; const Just = <A>(a: A) => ({ type: "Just", vars: [a] }); export const main = Just(1)
console.log(main)