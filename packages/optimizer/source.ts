import { InputMeta } from "./bindings/InputMeta";
import { LbfgsParams } from "./bindings/LbfgsParams";
import { OptState } from "./bindings/OptState";
import { OptStatus } from "./bindings/OptStatus";
import { Params } from "./bindings/Params";
import {
  penrose_call,
  penrose_gen_opt_problem,
  penrose_get_init_constraint_weight,
  penrose_init,
  penrose_step,
} from "./build/penrose_optimizer";
import optimizer from "./instance";

penrose_init();
const index = optimizer.__indirect_function_table.length;
optimizer.__indirect_function_table.grow(1);

export const importMemoryModule = "optimizer";
export const importMemoryName = "memory";

export const exportTableName = "builtins";
export const exportFunctionName = "f";

export const builtins = [
  "acos",
  "acosh",
  "asin",
  "asinh",
  "atan",
  "atanh",
  "atan2",
  "cbrt",
  "cos",
  "cosh",
  "exp",
  "expm1",
  "log",
  "log1p",
  "log10",
  "log2",
  "pow",
  "sin",
  "sinh",
  "tan",
  "tanh",
];

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
    builtins.forEach((name, i) => {
      const table = instance[exportTableName];
      if (table instanceof WebAssembly.Table)
        table.set(i, optimizer[`penrose_${name}`]);
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
  numInputs: number,
  numObjEngs: number,
  numConstrEngs: number
): Params => penrose_gen_opt_problem(numInputs, numObjEngs, numConstrEngs);

export type { InputMeta, LbfgsParams, OptState, OptStatus, Params };
