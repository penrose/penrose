import { InputKind } from "./bindings/InputKind";
import { LbfgsParams } from "./bindings/LbfgsParams";
import { OptState } from "./bindings/OptState";
import { OptStatus } from "./bindings/OptStatus";
import { Params } from "./bindings/Params";
import {
  InitOutput,
  penrose_call,
  penrose_gen_opt_problem,
  penrose_get_init_constraint_weight,
  penrose_init,
  penrose_step,
} from "./build/penrose_optimizer";
import { maybeOptimizer, optimizerReady } from "./instance";

let maybeIndex: number | undefined = undefined;

export const ready = optimizerReady.then(() => {
  penrose_init();
  maybeIndex = maybeOptimizer!.__indirect_function_table.length;
  maybeOptimizer!.__indirect_function_table.grow(1);
});

const getOptimizer = () => {
  if (maybeOptimizer === undefined || maybeIndex === undefined)
    throw Error("optimizer not initialized");
  return { optimizer: maybeOptimizer, index: maybeIndex };
};

export const importModule = "optimizer";
export const importMemoryName = "memory";

export const exportFunctionName = "f";

// https://github.com/rustwasm/wasm-bindgen/blob/f82f5c5852c3abf057bb737d545360a3a5c7d84c/crates/cli-support/src/wit/mod.rs#L1370-L1372
export const alignStackPointer = 16; // number of bytes

export type BuiltinType =
  | "addToStackPointer"
  | "unary"
  | "binary"
  | "polyRoots";

// use `InitOutput` type to satisfy typechecker
export const builtinsTyped: Map<keyof InitOutput, BuiltinType> = new Map([
  ["__wbindgen_add_to_stack_pointer", "addToStackPointer"],

  ["penrose_acos", "unary"],
  ["penrose_acosh", "unary"],
  ["penrose_asin", "unary"],
  ["penrose_asinh", "unary"],
  ["penrose_atan", "unary"],
  ["penrose_atanh", "unary"],
  ["penrose_cbrt", "unary"],
  ["penrose_cos", "unary"],
  ["penrose_cosh", "unary"],
  ["penrose_exp", "unary"],
  ["penrose_expm1", "unary"],
  ["penrose_log", "unary"],
  ["penrose_log1p", "unary"],
  ["penrose_log10", "unary"],
  ["penrose_log2", "unary"],
  ["penrose_sin", "unary"],
  ["penrose_sinh", "unary"],
  ["penrose_tan", "unary"],
  ["penrose_tanh", "unary"],
  ["penrose_inverse", "unary"],
  ["penrose_sign", "unary"],

  ["penrose_atan2", "binary"],
  ["penrose_pow", "binary"],

  ["penrose_poly_roots", "polyRoots"],
]);

// don't leak `InitOutput` type in interface
export const builtins: Map<string, BuiltinType> = builtinsTyped;

export interface Outputs<T> {
  gradient: T[]; // derivatives of primary output with respect to inputs
  primary: T;
  secondary: T[];
}

const makeImports = () => {
  const { optimizer } = getOptimizer();
  return {
    [importModule]: {
      [importMemoryName]: optimizer.memory,
      ...Object.fromEntries(
        [...builtinsTyped.keys()].map((name) => [name, optimizer[name]])
      ),
    },
  };
};

export class Gradient {
  private f: WebAssembly.ExportValue;
  private numSecondary: number;

  private constructor(instance: WebAssembly.Exports, numSecondary: number) {
    this.f = instance[exportFunctionName];
    this.numSecondary = numSecondary;
  }

  static async make(
    mod: WebAssembly.Module,
    numSecondary: number
  ): Promise<Gradient> {
    return new Gradient(
      (await WebAssembly.instantiate(mod, makeImports())).exports,
      numSecondary
    );
  }

  static makeSync(mod: WebAssembly.Module, numSecondary: number): Gradient {
    return new Gradient(
      new WebAssembly.Instance(mod, makeImports()).exports,
      numSecondary
    );
  }

  private link(): void {
    const { optimizer, index } = getOptimizer();
    optimizer.__indirect_function_table.set(index, this.f);
  }

  call(inputs: number[]): Outputs<number> {
    const { index } = getOptimizer();
    const gradient = new Float64Array(inputs.length);
    const secondary = new Float64Array(this.numSecondary);
    this.link();
    const primary = penrose_call(
      index,
      new Float64Array(inputs),
      gradient,
      secondary
    );
    return {
      gradient: Array.from(gradient),
      primary,
      secondary: Array.from(secondary),
    };
  }

  step(state: OptState, steps: number): OptState {
    const { index } = getOptimizer();
    this.link();
    return penrose_step(state, index, steps);
  }
}

export const getInitConstraintWeight = () => {
  getOptimizer();
  return penrose_get_init_constraint_weight();
};

export const genOptProblem = (
  inputKinds: InputKind[],
  numObjEngs: number,
  numConstrEngs: number
): Params => {
  getOptimizer();
  return penrose_gen_opt_problem(inputKinds, numObjEngs, numConstrEngs);
};

export type { InputKind, LbfgsParams, OptState, OptStatus, Params };
