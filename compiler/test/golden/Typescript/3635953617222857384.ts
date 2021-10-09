type Ident<A> = { type: "Ident"; vars: [A] };
const Ident = <A>(a: A) => ({ type: "Ident", vars: [a] });
const {
  vars: [a],
} = Ident(1);
export const main = a;
console.log(main);
