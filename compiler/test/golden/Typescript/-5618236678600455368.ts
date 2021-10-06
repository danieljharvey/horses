type Maybe<A> = { type: "Just", vars: [A] } | { type: "Nothing", vars: [] }; export const main = { type: "Nothing", vars: [] }
console.log(main)