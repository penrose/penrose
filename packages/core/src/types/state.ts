import { Params } from "@penrose/optimizer";
import { Canvas, InputMeta } from "../shapes/Samplers.js";
import { Shape } from "../shapes/Shapes.js";
import * as ad from "./ad.js";
import { A } from "./ast.js";
import { StyleWarning } from "./errors.js";
import { ConstrFn, ObjFn } from "./style.js";
import { WithContext } from "./styleSemantics.js";
import { FloatV } from "./value.js";

export type ShapeFn = (xs: number[]) => Shape<number>[];

export type OptPipeline = string[];

export type StagedConstraints = Map<string, ad.Masks>;

export interface InputInfo {
  handle: ad.Input;
  meta: InputMeta;
}

/**
 * The diagram state
 */
export interface State {
  warnings: StyleWarning[];
  variation: string;
  constraintSets: StagedConstraints;
  objFns: Fn[];
  constrFns: Fn[];
  varyingValues: number[];
  inputs: InputInfo[]; // same length as `varyingValues`
  labelCache: LabelCache;
  shapes: Shape<ad.Num>[];
  canvas: Canvas;
  gradient: ad.Gradient;
  currentStageIndex: number;
  optStages: string[];
  computeShapes: ShapeFn;
  params: Params;
}

/**
 * Output of label generation.
 */

export type LabelData = EquationData | TextData;
export interface EquationData {
  tag: "EquationData";
  width: FloatV<number>;
  height: FloatV<number>;
  descent: FloatV<number>;
  ascent: FloatV<number>;
  rendered: HTMLElement;
}

export interface TextData {
  tag: "TextData";
  width: FloatV<number>;
  height: FloatV<number>;
  descent: FloatV<number>;
  ascent: FloatV<number>;
}

export type LabelCache = Map<string, LabelData>;

export type OptStages = "All" | Set<string>;

/**
 * Generic export interface for constraint or objective functions
 */
export interface Fn {
  ast: WithContext<ObjFn<A> | ConstrFn<A>>;
  output: ad.Num;
  optStages: OptStages;
}
