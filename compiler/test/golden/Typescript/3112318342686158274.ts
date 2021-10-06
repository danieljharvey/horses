type Maybe<A> = { type: "Just", vars: [A] } | { type: "Nothing", vars: [] }; export const main = <A>(a: A) => ({ type: "Just", vars: [a] })
console.log(main)