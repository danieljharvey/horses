/* 
(q -> String -> o) -> q -> String -> q
 */
const stringReduce = (f) => (def) => (str) => {
  const match = (value) => {
    if (value === "") {
      return def;
    }
    if (value.length >= 1) {
      const head = value.charAt(0);
      const tail = value.slice(1);
      const nextVal = f(def)(head);
      return stringReduce(f)(nextVal)(tail);
    }
    throw new Error("Pattern match error");
  };
  return match(str);
};
export const main = stringReduce;
