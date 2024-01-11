import {
  Num,
  OptLabelCache,
  OptRenderState,
  PenroseError,
  Shape,
} from "@penrose/core";

/** request */

type ID = { id: string };

export type Req =
  | Init
  | ComputeShapes
  | ((Compile | RespLabels | Resample) & ID);

export type Resample = {
  tag: "Resample";
  variation: string;
};

export type ComputeShapes = {
  tag: "ComputeShapes";
  index: number;
};

export type Init = {
  tag: "Init";
  sharedMemory: SharedArrayBuffer;
};

export type RespLabels = {
  tag: "RespLabels";
  labelCache: OptLabelCache;
};

export type ReqLabels = {
  tag: "ReqLabels";
  shapes: Shape<Num>[];
};

export type Compile = {
  tag: "Compile";
  domain: string;
  style: string;
  substance: string;
  variation: string;
};

/** response */
export type Resp = Ready | ((Update | Error | Finished | ReqLabels) & ID);

export type Update = {
  tag: "Update";
  state: OptRenderState;
  stepsSoFar: number;
};

export type Finished = {
  tag: "Finished";
  state: OptRenderState;
};

export type Error = {
  tag: "Error";
  error: PenroseError;
};

export type Ready = {
  tag: "Ready";
};
