const { cat: b, dog: a } = { cat: 2, dog: 1 }; export const main = [a,b] as const
console.log(main)