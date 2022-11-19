import { penrose_call } from "./build/penrose_optimizer";
import optimizer from "./instance";

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
    this.link();
    const gradient = new Float64Array(inputs.length);
    const secondary = new Float64Array(this.numSecondary);
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
}

export interface OptState {
  varyingValues: number[];
  params: Params;
}

export type OptStatus =
  | "NewIter"
  | "UnconstrainedRunning"
  | "UnconstrainedConverged"
  | "EPConverged"
  | "Error";

// `n` is the size of the varying state
export interface LbfgsParams {
  lastState: number[] | undefined; // nx1 (col vec)
  lastGrad: number[] | undefined; // nx1 (col vec)
  s_list: number[][]; // list of nx1 col vecs
  y_list: number[][]; // list of nx1 col vecs
  numUnconstrSteps: number;
  memSize: number;
}

export interface FnEvaled {
  f: number;
  gradf: number[];
  objEngs: number[];
  constrEngs: number[];
}

export interface Params {
  optStatus: OptStatus;
  /** Constraint weight for exterior point method **/
  weight: number;
  /** Info for unconstrained optimization **/
  UOround: number;
  lastUOstate?: number[];
  lastUOenergy?: number;
  lastObjEnergies?: number[];
  lastConstrEnergies?: number[];

  /** Info for exterior point method **/
  EPround: number;
  lastEPstate?: number[];
  lastEPenergy?: number;

  lastGradient: number[]; // Value of gradient evaluated at the last state
  lastGradientPreconditioned: number[]; // Value of gradient evaluated at the last state, preconditioned by LBFGS
  // ^ Those two are stored to make them available to Style later

  // For L-BFGS
  lbfgsInfo: LbfgsParams;

  // Higher-order functions (not yet applied with hyperparameters, in this case, just the EP weight)
  objectiveAndGradient: (epWeight: number) => (xs: number[]) => FnEvaled;

  // Applied with weight (or hyperparameters in general) -- may change with the EP round
  currObjectiveAndGradient(xs: number[]): FnEvaled;
}

// TODO
export const step = (state: OptState, steps: number): OptState => state;
