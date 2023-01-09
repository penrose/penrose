import { PenroseError } from "@penrose/core";
import { OptState } from "@penrose/optimizer";

/** request */
export type Req = Init | Step;

export type Init = {
  tag: "Init";
  mod: WebAssembly.Module;
  numAddends: number;
  numSecondary: number;
};

export type Step = {
  tag: "Step";
  state: OptState;
  numSteps: number;
};

/** response */
export type Resp = Success | State | Error;

export type Success = {
  tag: "Success";
};

export type State = {
  tag: "State";
  state: OptState;
};

export type Error = {
  tag: "Error";
  error: PenroseError;
};
