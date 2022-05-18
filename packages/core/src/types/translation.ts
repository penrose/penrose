import * as ad from "./ad";
import { StyleResults } from "./errors";
import { ArgVal, GPI } from "./value";

export interface Translation {
  diagnostics: StyleResults;
  symbols: Map<string, Named>;
  shapes: GPI<ad.Num>["contents"][];
  objectives: ad.Num[];
  constraints: ad.Num[];
  layering: [string, string][];
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
