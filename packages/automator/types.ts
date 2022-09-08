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
  substanceName: string;
  styleName: string;
  domainName: string;
  id: string;
  renderedOn: number;
  timeTaken: TimeTaken;
  selectorMatches: [];
  optProblem: OptProblem;
  reference: Reference;
  ciee: number | null; // JSON turns Infinity into null
  extra?: unknown;
}

export interface AggregateData {
  [key: string]: InstanceData;
}
