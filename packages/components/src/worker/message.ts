import { PenroseError, StagedState } from "@penrose/core";

/** request */
export type Req = Init | Step | StepUntilConvergence | StepNextStage;

export type Init = {
  tag: "Init";
  mod: WebAssembly.Module;
  numAddends: number;
  numSecondary: number;
};

export type Step = {
  tag: "Step";
  state: StagedState;
  numSteps: number;
};

export type StepUntilConvergence = {
  tag: "StepUntilConvergence";
  state: StagedState;
  numSteps: number;
};

export type StepNextStage = {
  tag: "StepNextStage";
  state: StagedState;
  numSteps: number;
};

/** response */
export type Resp = Success | State | Error;

export type Success = {
  tag: "Success";
};

export type State = {
  tag: "State";
  state: StagedState;
};

export type Error = {
  tag: "Error";
  error: PenroseError;
};
