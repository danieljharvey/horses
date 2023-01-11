export const equals_ = <A>(a: A) => (b: A) =>
  JSON.stringify(a) == JSON.stringify(b);

export type Tramp_<A> = { func: () => Tramp_<A> } | { value: A };

export const next_ = <A>(func: () => Tramp_<A>): Tramp_<A> => ({
  func,
});

export const done_ = <A>(value: A): Tramp_<A> => ({ value });

export const trampoline_ = <A>(first: Tramp_<A>): A => {
  let result = first;
  while ("func" in result) {
    result = result.func();
  }
  return result.value;
};
