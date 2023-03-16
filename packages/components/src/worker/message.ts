import { PenroseError, PenroseState } from "@penrose/core";
import { StyleWarning } from "@penrose/core/dist/types/errors";

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
  state: SVGSVGElement;
};

export type Finished = {
  tag: "Finished";
  state: string;
};

export type Error = {
  tag: "Error";
  error: PenroseError;
};

export type ReadyForNewTrio = {
  tag: "ReadyForNewTrio";
};
