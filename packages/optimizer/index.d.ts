export type InputMeta = "sampler" | "pending";

export interface State {
  inputs: InputMeta[];
  numObjEngs: number;
  numConstrEngs: number;
  varyingValues: number[];
}

export interface Optimizer {
  link: (source: Uint8Array) => Promise<void>;
  converge: (state: State) => number[];
}

export declare const importMemoryModule: string;
export declare const importMemoryName: string;

export declare const exportTableName: string;
export declare const exportFunctionName: string;

export declare const builtins: string[];

export declare const getOptimizer: () => Promise<Optimizer>;
