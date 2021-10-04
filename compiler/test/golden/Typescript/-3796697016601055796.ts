type Maybe<A> = { type: "Just", vars: [A] } | { type: "Nothing", vars: [] }; export const main = { type: "Just", vars: [1] }
console.log(main)