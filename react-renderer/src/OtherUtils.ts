import * as _ from "lodash";

export const normList = (xs: number[]) =>
  Math.sqrt(_.sum(xs.map(e => e * e)));

export function repeat<T>(i: number, x: T) {
  const xs = [];

  for (let j = 0; j < i; j++) {
    xs.push(x);
  };

  return xs;
};

export const all = (xs: boolean[]) =>
  xs.reduce((prev, curr) => prev && curr, true);
