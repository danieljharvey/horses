export const main = <C,D>(a: readonly [C,D]) => { const [b,c] = a; return b; }
console.log(main)