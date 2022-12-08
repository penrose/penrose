import { InputKind } from "./bindings/InputKind";
import { LbfgsParams } from "./bindings/LbfgsParams";
import { OptState } from "./bindings/OptState";
import { OptStatus } from "./bindings/OptStatus";
import { Params } from "./bindings/Params";
import init, {
  InitOutput,
  penrose_call,
  penrose_gen_opt_problem,
  penrose_get_init_constraint_weight,
  penrose_init,
  penrose_step,
} from "./build/penrose_optimizer";

const optimizer = await init();
penrose_init();
const index = optimizer.__indirect_function_table.length;
optimizer.__indirect_function_table.grow(1);

export const importMemoryModule = "optimizer";
export const importMemoryName = "memory";

export const exportTableName = "builtins";
export const exportFunctionName = "f";

// https://github.com/rustwasm/wasm-bindgen/blob/f82f5c5852c3abf057bb737d545360a3a5c7d84c/crates/cli-support/src/wit/mod.rs#L1370-L1372
export const alignStackPointer = 16; // number of bytes

// use `InitOutput` type to satisfy typechecker
export const builtinsTyped: (keyof InitOutput)[] = [
  "__wbindgen_add_to_stack_pointer",

  "penrose_acos",
  "penrose_acosh",
  "penrose_asin",
  "penrose_asinh",
  "penrose_atan",
  "penrose_atanh",
  "penrose_atan2",
  "penrose_cbrt",
  "penrose_cos",
  "penrose_cosh",
  "penrose_exp",
  "penrose_expm1",
  "penrose_log",
  "penrose_log1p",
  "penrose_log10",
  "penrose_log2",
  "penrose_pow",
  "penrose_sin",
  "penrose_sinh",
  "penrose_tan",
  "penrose_tanh",

  "penrose_poly_roots",
];

// don't leak `InitOutput` type in interface
export const builtins: string[] = builtinsTyped;

export interface Outputs<T> {
  gradient: T[]; // derivatives of primary output with respect to inputs
  primary: T;
  secondary: T[];
}

export class Gradient {
  private f: WebAssembly.ExportValue;
  private numSecondary: number;

  constructor(mod: WebAssembly.Module, numSecondary: number) {
    const instance = new WebAssembly.Instance(mod, {
      [importMemoryModule]: { [importMemoryName]: optimizer.memory },
    }).exports;
    builtinsTyped.forEach((name, i) => {
      const table = instance[exportTableName];
      if (table instanceof WebAssembly.Table) table.set(i, optimizer[name]);
    });
    this.f = instance[exportFunctionName];
    this.numSecondary = numSecondary;
  }

  private link() {
    optimizer.__indirect_function_table.set(index, this.f);
  }

  call(inputs: number[]): Outputs<number> {
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
    this.link();
    return penrose_step(state, index, steps);
  }
}

export const initConstraintWeight = penrose_get_init_constraint_weight();

export const genOptProblem = (
  inputKinds: InputKind[],
  numObjEngs: number,
  numConstrEngs: number
): Params => penrose_gen_opt_problem(inputKinds, numObjEngs, numConstrEngs);

export type { InputKind, LbfgsParams, OptState, OptStatus, Params };
