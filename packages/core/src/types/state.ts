import { Params } from "../engine/Optimizer.js";
import { Canvas, InputMeta } from "../shapes/Samplers.js";
import { Shape } from "../shapes/Shapes.js";
import * as ad from "./ad.js";
import { A } from "./ast.js";
import { StyleWarning } from "./errors.js";
import { ConstrFn, ObjFn } from "./style.js";
import { WithContext } from "./styleSemantics.js";
import { ArgVal, FloatV, Value } from "./value.js";
import { ShapeT } from "./types.js";

export type ShapeFn = (xs: number[]) => Shape<number>[];

export type OptPipeline = string[];

export type StagedConstraints = Map<string, ad.Masks>;

/**
 * For each path with a some optimized value, get that
 */
export type IdxsByPath = Map<string, ArgVal<number | undefined>>

export interface InputInfo {
  handle: ad.Var;
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
  currentStageIndex: number;
  optStages: string[];
  params: Params;
  gradient: ad.Gradient;
  computeShapes: ShapeFn;
  inputIdxsByPath: IdxsByPath;
  draggableShapePaths: Set<string>;
  pinnedInputIdxs: Set<number>;
}

/**
 * Output of label generation.
 */

export type LabelData = EquationData | TextData;
export type EquationData = {
  tag: "EquationData";
} & EquationMeasurement;

export interface EquationShape {
  rendered: HTMLElement;
}

export interface EquationMeasurement {
  width: FloatV<number>;
  height: FloatV<number>;
  descent: FloatV<number>;
  ascent: FloatV<number>;
}

export type TextData = {
  tag: "TextData";
} & TextMeasurement;

export interface TextMeasurement {
  width: FloatV<number>;
  height: FloatV<number>;
  descent: FloatV<number>;
  ascent: FloatV<number>;
}

// Cache of label measurements and MathJax-rendered math labels for `Equation`s
export type LabelCache = Map<string, (EquationData & EquationShape) | TextData>;

// A Label Cache but without the rendered HTMLElement
export type LabelMeasurements = Map<string, LabelData>;

export type OptStages = "All" | Set<string>;

/**
 * Generic export interface for constraint or objective functions
 */
export interface Fn {
  ast: WithContext<ObjFn<A> | ConstrFn<A>>;
  output: ad.Num;
  optStages: OptStages;
}
