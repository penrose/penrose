import { PenroseError, PenroseState } from "@penrose/core";

/** request */
export type Req = Init | Compile;

export type Init = {
  tag: "Init";
  sharedMemory: SharedArrayBuffer;
};

export type Compile = {
  tag: "Compile";
  domain: string;
  style: string;
  substance: string;
  variation: string;
};

/** response */
export type Resp = Update | Error | ReadyForNewTrio | Finished;

export type Update = {
  tag: "Update";
  state: PenroseState;
};

export type Finished = {
  tag: "Finished";
  state: PenroseState;
};

export type Error = {
  tag: "Error";
  error: PenroseError;
};

export type ReadyForNewTrio = {
  tag: "ReadyForNewTrio";
};
