export const __patternMatch = (val, patterns) => {
  const checked = patterns.map(([pat,expr]) => [pat(val),expr])
  const match = checked.find(([a,_])=>a)
  if (match === undefined) {
    throw new Error("pattern matching broken")
  }
  return match[1](match[0])
}

export const __concat = (a,b) => [...a,...b]

// very cheap eq function, forgive me padre
export const __eq = (a, b) => JSON.stringify(a) === JSON.stringify(b);
