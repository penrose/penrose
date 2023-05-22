export interface TimeTaken {
  overall: number;
  compilation: number;
  labelling: number;
  optimization: number;
  rendering: number;
}

export interface OptProblem {
  constraintCount: number;
  objectiveCount: number;
}

export interface Reference {
  substance: string;
  style: string;
  domain: string;
  variation: string;
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

export interface AggregateData {
  [key: string]: InstanceData;
}
