export interface Optimizer {
  link: (source: Uint8Array) => Promise<void>;
  step: (x: number) => number;
}

export declare const importMemoryModule: string;
export declare const importMemoryName: string;

export declare const exportTableName: string;
export declare const exportFunctionName: string;

export declare const builtins: string[];

export declare const getOptimizer: () => Promise<Optimizer>;
