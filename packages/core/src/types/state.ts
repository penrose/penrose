import { Gradient, OptState } from "@penrose/optimizer";
import { Canvas, InputMeta } from "shapes/Samplers";
import * as ad from "types/ad";
import { A } from "./ast";
import { StyleWarning } from "./errors";
import { Shape, ShapeAD } from "./shape";
import { ConstrFn, ObjFn } from "./style";
import { WithContext } from "./styleSemantics";
import { FloatV } from "./value";

export type ShapeFn = (xs: number[]) => Shape[];

export interface StagedFns {
  [k: string]: Fn[];
}
export interface StagedConstraints {
  [k: string]: { objFns: Fn[]; constrFns: Fn[] };
}

/**
 * The diagram state
 */
export interface State extends OptState {
  warnings: StyleWarning[];
  variation: string;
  constraintSets: StagedConstraints;
  objFns: Fn[];
  constrFns: Fn[];
  inputs: InputMeta[]; // same length as `varyingValues`
  labelCache: LabelCache;
  shapes: ShapeAD[];
  canvas: Canvas;
  gradient: Gradient;
  computeShapes: ShapeFn;
}

/**
 * Output of label generation.
 */

export type LabelData = EquationData | TextData;
export interface EquationData {
  tag: "EquationData";
  width: FloatV<number>;
  height: FloatV<number>;
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

/**
 * Generic export interface for constraint or objective functions
 */

export type OptStage = "ShapeLayout" | "LabelLayout" | "Overall";
export interface Fn {
  ast: WithContext<ObjFn<A> | ConstrFn<A>>;
  output: ad.Num;
  optStages: OptStage[];
}
