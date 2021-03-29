// pattern matching for data types
const objectMatch = (val, matches, catchAll) => {
  const branch = matches[val.type];
  if (!branch) {
    return catchAll;
  }
  if (val.vars.length == 0) {
    return branch;
  }
  return val.vars.reduce((f, a) => f(a), branch);
};

// built in pattern matching to split head and tail of string
const stringMatch = (val, matches, catchAll) => {
  if (val.length > 0 && "StrHead" in matches) {
    return matches["StrHead"](val[0])(val.slice(1));
  } else if (val.length === 0 && "StrEmpty" in matches) {
    return matches["StrEmpty"];
  }
  return catchAll;
};

// built in pattern matching to split head and tail of array
const arrayMatch = (val, matches, catchAll) => {
  if (val.length > 0 && "ArrHead" in matches) {
    return matches["ArrHead"](val[0])(val.slice(1));
  } else if (val.length === 0 && "ArrEmpty" in matches) {
    return matches["ArrEmpty"];
  }
  return catchAll;
};

const __match = (val, matches, catchAll) =>
  typeof val === "string"
    ? stringMatch(val, matches, catchAll)
    : Array.isArray(val)
    ? arrayMatch(val, matches, catchAll)
    : objectMatch(val, matches, catchAll);

// very cheap eq function, forgive me padre
const __eq = (a, b) => JSON.stringify(a) === JSON.stringify(b);

module.exports = {
  __match,
  __eq,
};
