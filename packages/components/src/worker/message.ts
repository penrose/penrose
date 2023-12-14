import { PenroseError, RenderState } from "@penrose/core";
import { ShapeAD } from "@penrose/core/dist/types/shape";
import { OptLabelCache, OptRenderState } from "@penrose/core/dist/types/state";

/** request */
export type Req = Init | Compile | RespLabelCache;

export type Init = {
  tag: "Init";
  sharedMemory: SharedArrayBuffer;
};

export type RespLabelCache = {
  tag: "RespLabelCache"
  labelCache: OptLabelCache
}

export type ReqLabelCache = {
  tag: "ReqLabelCache";
  shapes: ShapeAD[];
}

export type Compile = {
  tag: "Compile";
  domain: string;
  style: string;
  substance: string;
  variation: string;
};

/** response */
export type Resp = Update | Error | ReadyForNewTrio | Finished | ReqLabelCache;

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

export type ReadyForNewTrio = {
  tag: "ReadyForNewTrio";
};
