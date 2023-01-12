export const equals_ = (a) => (b) => JSON.stringify(a) == JSON.stringify(b);

export const next_ = (func) => ({
  func,
});

export const done_ = (value) => ({ value });

export const trampoline_ = (first) => {
  let result = first;
  while ("func" in result) {
    result = result.func();
  }
  return result.value;
};
