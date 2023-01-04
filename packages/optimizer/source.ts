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

/**
 * Wait for the optimizer to be loaded and initialized. Most functions exported
 * from this package need this promise to be resolved before they can be called.
 */
export const ready = optimizerReady.then(() => {
  penrose_init();
  // we pass `--keep-lld-exports` to `wasm-bindgen` because we need access to
  // this `__indirect_function_table` to allow us to swap in different gradient
  // functions at runtime
  maybeIndex = maybeOptimizer!.__indirect_function_table.length;
  maybeOptimizer!.__indirect_function_table.grow(1);
});

const getOptimizer = () => {
  if (maybeOptimizer === undefined || maybeIndex === undefined)
    throw Error("optimizer not initialized");
  return { optimizer: maybeOptimizer, index: maybeIndex };
};

/**
 * The module name of every import in a module used to construct a `Gradient`.
 */
export const importModule = "";
/**
 * The name of the memory import in a module used to construct a `Gradient`.
 */
export const importMemoryName = "";

/**
 * The name of the function exported by a module used to construct a `Gradient`.
 */
export const exportFunctionName = "";

// https://github.com/rustwasm/wasm-bindgen/blob/f82f5c5852c3abf057bb737d545360a3a5c7d84c/crates/cli-support/src/wit/mod.rs#L1370-L1372
/**
 * The number of bytes to which the stack pointer must always be aligned, when
 * using the `__wbindgen_add_to_stack_pointer` built-in.
 */
export const alignStackPointer = 16;

/**
 * Symbolic names for the types of built-in `Gradient` functions.
 */
export type BuiltinType =
  | "addToStackPointer"
  | "unary"
  | "binary"
  | "polyRoots";

// use `InitOutput` type to satisfy typechecker
const builtinsTyped: Map<keyof InitOutput, BuiltinType> = new Map([
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
/**
 * A map from names of all built-in `Gradient` functions to the names of their
 * types. The names themselves don't matter; they're purely for human
 * readability purposes. The key insertion order does matter, as it is used to
 * determine the name of each individual import.
 */
export const builtins: Map<string, BuiltinType> = builtinsTyped;

/**
 * A structure used to collect the various outputs of a `Gradient` function.
 * This is generic in the concrete number type, because it can also be useful in
 * situations where the elements are, for instance, computation graph nodes.
 */
export interface Outputs<T> {
  /** Derivatives of primary output with respect to inputs. */
  gradient: T[];
  /** Primary output. */
  primary: T;
  /** Secondary outputs. */
  secondary: T[];
}

const makeImports = () => {
  const { optimizer } = getOptimizer();
  return {
    [importModule]: {
      [importMemoryName]: optimizer.memory,
      ...Object.fromEntries(
        [...builtinsTyped.keys()].map((name, i) => [
          i.toString(36),
          optimizer[name],
        ])
      ),
    },
  };
};

/**
 * An instantiated WebAssembly function that can be used either to directly
 * compute outputs and gradients or to step a state via the optimizer.
 *
 * Each WebAssembly module used to construct an instance of this class must
 * follow some conventions using other things exported from this package:
 *
 * - The module name of every import must match `importModule`.
 *
 * - One import must be a memory whose name matches `importMemoryName`.
 *
 * - The rest of the imports must be functions in the same order as the the
 *   insertion order of `builtins`. The name of each of these imports must be a
 *   base-36 integer (`"0"`-`"9"`, `"a"`-`"z"`) corresponding to the index of
 *   the name in `builtins`. The type of the import is determined by its value
 *   in `builtins`, which is an element of `BuiltinType`:
 *
 *   - `"addToStackPointer"` takes one `i32` and returns one `i32`.
 *   - `"unary"` takes one `f64` and returns one `f64`.
 *   - `"binary"` takes two `f64`s and returns one `f64`.
 *   - `"polyRoots"` takes two `i32`s and returns nothing.
 *
 * - The module must export a function whose name matches `exportFunctionName`,
 *   which takes three `i32`s and returns one `f64`.
 */
export class Gradient {
  private f: WebAssembly.ExportValue;
  private numSecondary: number;

  private constructor(instance: WebAssembly.Exports, numSecondary: number) {
    this.f = instance[exportFunctionName];
    this.numSecondary = numSecondary;
  }

  /**
   * `ready` must be resolved first.
   * @param mod a compiled Wasm module following the conventions of this class
   * @param numSecondary the number of secondary outputs
   * @returns a usable `Gradient` object initialized with all necessary builtins
   */
  static async make(
    mod: WebAssembly.Module,
    numSecondary: number
  ): Promise<Gradient> {
    return new Gradient(
      (await WebAssembly.instantiate(mod, makeImports())).exports,
      numSecondary
    );
  }

  /**
   * Synchronous version of `make`; everything from its docstring applies here.
   */
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

  /**
   * @param inputs to the function
   * @returns the `primary` output, its `gradient`, and any `secondary` outputs
   */
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

  /**
   * @param state current state
   * @param steps how many to take
   * @returns updated state
   */
  step(state: OptState, steps: number): OptState {
    const { index } = getOptimizer();
    this.link();
    return penrose_step(state, index, steps);
  }
}

/**
 * `ready` must be resolved first.
 * @returns the initial weight for constraints
 */
export const getInitConstraintWeight = () => {
  getOptimizer();
  return penrose_get_init_constraint_weight();
};

/**
 * `ready` must be resolved first.
 * @param inputKinds whether each varying value index should be optimized
 * @param numObjEngs the number of objectives in this optimization problem
 * @param numConstrEngs the number of constraints in this optimization problem
 * @returns initial optimization parameters
 */
export const genOptProblem = (
  inputKinds: InputKind[],
  numObjEngs: number,
  numConstrEngs: number
): Params => {
  getOptimizer();
  return penrose_gen_opt_problem(inputKinds, numObjEngs, numConstrEngs);
};

export type { InputKind, LbfgsParams, OptState, OptStatus, Params };
