import {
  Binary,
  Bool,
  Comp,
  Logic,
  Nary,
  Num,
  Unary,
  Vec,
} from "@penrose/core";
import { polyRoots } from "@penrose/optimizer";
import { Accessor, createMemo } from "solid-js";

const evalComp = (op: Comp["binop"]): ((x: number, y: number) => boolean) => {
  switch (op) {
    case ">": {
      return (x, y) => x > y;
    }
    case "<": {
      return (x, y) => x < y;
    }
    case "===": {
      return (x, y) => x === y;
    }
    case ">=": {
      return (x, y) => x >= y;
    }
    case "<=": {
      return (x, y) => x <= y;
    }
  }
};

const evalLogic = (
  op: Logic["binop"]
): ((x: boolean, y: boolean) => boolean) => {
  switch (op) {
    case "&&": {
      return (x, y) => x && y;
    }
    case "||": {
      return (x, y) => x || y;
    }
    case "!==": {
      return (x, y) => x !== y;
    }
  }
};

const evalUnary = (op: Unary["unop"]): ((x: number) => number) => {
  switch (op) {
    case "inverse": {
      return (x) => 1 / x;
    }
    case "neg": {
      return (x) => -x;
    }
    case "squared": {
      return (x) => x * x;
    }
    case "abs":
    case "acosh":
    case "acos":
    case "asin":
    case "asinh":
    case "atan":
    case "atanh":
    case "cbrt":
    case "ceil":
    case "cos":
    case "cosh":
    case "exp":
    case "expm1":
    case "floor":
    case "log":
    case "log2":
    case "log10":
    case "log1p":
    case "round":
    case "sign":
    case "sin":
    case "sinh":
    case "sqrt":
    case "tan":
    case "tanh":
    case "trunc": {
      return Math[op];
    }
  }
};

const evalBinary = (
  op: Binary["binop"]
): ((x: number, y: number) => number) => {
  switch (op) {
    case "+": {
      return (x, y) => x + y;
    }
    case "*": {
      return (x, y) => x * y;
    }
    case "-": {
      return (x, y) => x - y;
    }
    case "/": {
      return (x, y) => x / y;
    }
    case "atan2":
    case "max":
    case "min":
    case "pow": {
      return Math[op];
    }
  }
};

const evalNary = (op: Nary["op"]): ((xs: number[]) => number) => {
  switch (op) {
    case "addN": {
      return (xs) => xs.reduce((x, y) => x + y, 0);
    }
    case "maxN": {
      return (xs) => Math.max(...xs);
    }
    case "minN": {
      return (xs) => Math.min(...xs);
    }
  }
};

const secret = Symbol("signal");

const boolWith = (x: Bool, signal: Accessor<boolean>): Accessor<boolean> => {
  const mem = createMemo(signal);
  (x as any)[secret] = mem;
  return mem;
};

const numWith = (
  x: Exclude<Num, number>,
  signal: Accessor<number>
): Accessor<number> => {
  const mem = createMemo(signal);
  (x as any)[secret] = mem;
  return mem;
};

const vecWith = (x: Vec, signal: Accessor<number[]>): Accessor<number[]> => {
  const mem = createMemo(signal);
  (x as any)[secret] = mem;
  return mem;
};

// TODO: rewrite these from recursive to iterative

export const boolSignal = (x: Bool): Accessor<boolean> => {
  if (secret in x) return x[secret] as Accessor<boolean>;
  switch (x.tag) {
    case "Comp": {
      const { binop, left, right } = x;
      const y = numSignal(left);
      const z = numSignal(right);
      const f = evalComp(binop);
      return boolWith(x, () => f(y(), z()));
    }
    case "Logic": {
      const { binop, left, right } = x;
      const y = boolSignal(left);
      const z = boolSignal(right);
      const f = evalLogic(binop);
      return boolWith(x, () => f(y(), z()));
    }
    case "Not": {
      const { param } = x;
      const y = boolSignal(param);
      return boolWith(x, () => !y());
    }
  }
};

export const numSignal = (x: Num): Accessor<number> => {
  if (typeof x === "number") return () => x;
  if (secret in x) return x[secret] as Accessor<number>;
  switch (x.tag) {
    case "Var": {
      return () => x.val;
    }
    case "Unary": {
      const y = numSignal(x.param);
      const f = evalUnary(x.unop);
      return numWith(x, () => f(y()));
    }
    case "Binary": {
      const y = numSignal(x.left);
      const z = numSignal(x.right);
      const f = evalBinary(x.binop);
      return numWith(x, () => f(y(), z()));
    }
    case "Ternary": {
      const y = boolSignal(x.cond);
      const z = numSignal(x.then);
      const w = numSignal(x.els);
      return numWith(x, () => (y() ? z() : w()));
    }
    case "Nary": {
      const ys = x.params.map(numSignal);
      const f = evalNary(x.op);
      return numWith(x, () => f(ys.map((y) => y())));
    }
    case "Index": {
      const { index, vec } = x;
      const v = vecSignal(vec);
      return numWith(x, () => v()[index]);
    }
  }
};

const vecSignal = (x: Vec): Accessor<number[]> => {
  if (secret in x) return x[secret] as Accessor<number[]>;
  switch (x.tag) {
    case "PolyRoots": {
      const ys = x.coeffs.map(numSignal);
      const v = new Float64Array(x.degree);
      return vecWith(x, () => {
        ys.forEach((y, i) => {
          v[i] = y();
        });
        polyRoots(v);
        return Array.from(v);
      });
    }
  }
};