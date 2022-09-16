export interface OptimizerExports {
  __indirect_function_table: WebAssembly.Table;
  memory: WebAssembly.Memory;
  step: (f: number, x: f64) => number;
}

export declare const getOptimizer: () => Promise<OptimizerExports>;
