
export const That = (b) => ({ type: "That", vars: [b] }); export const These = (a) => (b) => ({ type: "These", vars: [a,b] }); export const This = (a) => ({ type: "This", vars: [a] }); 