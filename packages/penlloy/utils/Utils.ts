export const safeArray = <T>(x: T | T[] | undefined): T[] => {
  if (x === undefined) {
    return [];
  } else if (Array.isArray(x)) {
    return x;
  } else return [x];
};

export const safe = <T>(x: T | undefined): T => {
  if (x === undefined) {
    throw new Error("Found x to be undefined");
  } else return x;
};
