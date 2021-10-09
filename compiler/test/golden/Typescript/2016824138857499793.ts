type Maybe<A> = { type: "Just", vars: [A] } | { type: "Nothing", vars: [] }; const Nothing = { type: "Nothing", vars: [] }; export const main = Nothing
console.log(main)