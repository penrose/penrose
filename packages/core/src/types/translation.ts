import * as im from "immutable";
import * as ad from "./ad";
import { StyleResults } from "./errors";
import { ShapeAD } from "./shape";
import { ArgVal } from "./value";

export type StyleSymbols = im.Map<string, Named>;
export interface Translation {
  diagnostics: StyleResults;
  symbols: StyleSymbols;
  shapes: im.List<ShapeAD>;
  varying: im.List<ad.Input>;
  objectives: im.List<ad.Num>;
  constraints: im.List<ad.Num>;
  layering: im.List<[string, string]>;
}

export type Named = ArgVal<ad.Num> | Obj | Constr | Layer;

export interface Obj {
  tag: "Obj";
  output: ad.Num;
}

export interface Constr {
  tag: "Constr";
  output: ad.Num;
}

export interface Layer {
  tag: "Layer";
  below: string;
  above: string;
}
