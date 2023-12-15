import {
  Num,
  OptLabelCache,
  OptRenderState,
  PenroseError,
  Shape,
} from "@penrose/core";

/** request */
export type Req = Init | Compile | RenderedLabels | Resample;

export type Resample = {
  tag: "Resample";
  variation: string;
};

export type Init = {
  tag: "Init";
  sharedMemory: SharedArrayBuffer;
};

export type RenderedLabels = {
  tag: "RespLabelCache";
  labelCache: OptLabelCache;
};

export type RequestLabels = {
  tag: "ReqLabelCache";
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
export type Resp = Update | Error | Ready | Finished | RequestLabels;

export type Update = {
  tag: "Update";
  state: OptRenderState;
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
