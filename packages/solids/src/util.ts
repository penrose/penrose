import {
  Binary,
  Bool,
  Comp,
  Index,
  Logic,
  Nary,
  Num,
  Ternary,
  Unary,
  Var,
  Vec,
  variable,
} from "@penrose/core";
import { polyRoots } from "@penrose/optimizer";
import seedrandom from "seedrandom";
import { Accessor, createEffect, createMemo, on } from "solid-js";
import { SetStoreFunction, createStore } from "solid-js/store";

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
  op: Logic["binop"],
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
  op: Binary["binop"],
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

interface BoolSignal {
  [secret]: Accessor<boolean>;
}

interface SignalComp extends Comp, BoolSignal {}
interface SignalLogic extends Logic, BoolSignal {}
interface SignalNot extends BoolSignal {}
export type SignalBool = SignalComp | SignalLogic | SignalNot;

interface NumSignal {
  [secret]: Accessor<number>;
}

interface SignalUnary extends Unary, NumSignal {}
interface SignalBinary extends Binary, NumSignal {}
interface SignalTernary extends Ternary, NumSignal {}
interface SignalNary extends Nary, NumSignal {}
interface SignalIndex extends Index, NumSignal {}
export type SignalNum =
  | number
  | Var
  | SignalUnary
  | SignalBinary
  | SignalTernary
  | SignalNary
  | SignalIndex;

export const bool = (x: SignalBool): boolean => x[secret]();

export const num = (x: SignalNum): number => {
  if (typeof x === "number") return x;
  if (x.tag === "Var") return x.val;
  return x[secret]();
};

const boolWith = (x: Bool, signal: Accessor<boolean>): Accessor<boolean> => {
  const mem = createMemo(signal);
  (x as any)[secret] = mem;
  return mem;
};

const numWith = (
  x: Exclude<Num, number>,
  signal: Accessor<number>,
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

const boolSignal = (x: Bool): Accessor<boolean> => {
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

const numSignal = (x: Num): Accessor<number> => {
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

export const signalNum = (x: Num): SignalNum => {
  if (typeof x === "number") return x;
  numSignal(x);
  return x as SignalNum;
};

export const signalBool = (x: Bool): SignalBool => {
  boolSignal(x);
  return x as SignalBool;
};

export type Sampler = (x: number) => number;

export const sample = <T>(
  seed: Accessor<string>,
  f: (makeVar: (sampler: Sampler) => Var) => T,
): T => {
  const vars: { sampler: Sampler; setter: SetStoreFunction<Var> }[] = [];
  const rng = seedrandom(seed());
  let ok = true;
  const res = f((sampler) => {
    if (!ok) throw Error("can't keep sampling after scope is done");
    const [x, setter] = createStore(variable(sampler(rng())));
    vars.push({ sampler, setter });
    return x;
  });
  ok = false;
  createEffect(
    on(
      seed,
      (s) => {
        const rng = seedrandom(s);
        vars.forEach(({ sampler, setter }) => setter({ val: sampler(rng()) }));
      },
      { defer: true },
    ),
  );
  return res;
};
