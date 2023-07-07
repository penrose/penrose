export interface TimeTaken {
  overall: number;
  compilation: number;
  optimization: number;
  rendering: number;
}

export interface OptProblem {
  constraintCount: number;
  objectiveCount: number;
}

export interface InstanceData {
  substanceName?: string;
  styleNames?: string[];
  domainName?: string;
  id: string;
  renderedOn: number;
  timeTaken: TimeTaken;
  selectorMatches: [];
  optProblem: OptProblem;
}
