import { PenroseError, RenderState } from "@penrose/core";

/** request */
export type Req = Init | Compile;

export type Init = {
  tag: "Init";
  sharedMemory: SharedArrayBuffer;
  offscreenCanvas: OffscreenCanvas;
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
  state: RenderState;
};

export type Finished = {
  tag: "Finished";
  state: RenderState;
};

export type Error = {
  tag: "Error";
  error: PenroseError;
};

export type ReadyForNewTrio = {
  tag: "ReadyForNewTrio";
};
