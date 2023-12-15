import { Params } from "../engine/Optimizer.js";
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
  gradient: ad.Gradient;
  currentStageIndex: number;
  optStages: string[];
  computeShapes: ShapeFn;
  params: Params;
}

export interface OptEquationData {
  tag: "OptEquationData";
  width: FloatV<number>;
  height: FloatV<number>;
  descent: FloatV<number>;
  ascent: FloatV<number>;
}

export type OptLabelData = OptEquationData | TextData;

// A Label Cache but without the rendered HTMLElement because an HTMLElement can't
// be cloned to the worker thread
export type OptLabelCache = Map<string, OptLabelData>;

// State optimizer responds with for main thread to render
export interface OptRenderState {
  variation: string;
  labelCache: OptLabelCache;
  canvas: Canvas;
  shapes: Shape<number>[];
}

// Same as OptRenderState, but includes the rendered label SVG in the label cache.
// Rendered SVG is saved on main thread while worker optimizes since it can't be cloned.
// When worker sends back OptRenderState, it gets converted to RenderState on main thread
export interface RenderState {
  variation: string;
  labelCache: LabelCache;
  canvas: Canvas;
  shapes: Shape<number>[];
}

export const stateToOptRenderState = (state: State): OptRenderState => {
  return {
    variation: state.variation,
    labelCache: labelCacheToOptLabelCache(state.labelCache).optLabelCache,
    canvas: state.canvas,
    shapes: state.computeShapes(state.varyingValues),
  };
};

const labelDataToOptLabelData = (labelData: LabelData): OptLabelData => {
  if (labelData.tag === "EquationData") {
    return {
      tag: "OptEquationData",
      width: labelData.width,
      height: labelData.height,
      descent: labelData.descent,
      ascent: labelData.ascent,
    };
  }
  return labelData;
};

export const labelCacheToOptLabelCache = (
  labelCache: LabelCache,
): { optLabelCache: OptLabelCache; svgCache: Map<string, HTMLElement> } => {
  const optLabelCache: OptLabelCache = new Map();
  const svgCache: Map<string, HTMLElement> = new Map();

  labelCache.forEach((value, key) => {
    optLabelCache.set(key, labelDataToOptLabelData(value));
    if (value.tag === "EquationData") {
      svgCache.set(key, value.rendered);
    }
  });

  return { optLabelCache, svgCache };
};

export const optLabelCacheToLabelCache = (
  optLabelCache: OptLabelCache,
  svgCache: Map<string, HTMLElement>,
): LabelCache => {
  const labelCache: LabelCache = new Map();

  optLabelCache.forEach((value, key) => {
    if (value.tag === "OptEquationData") {
      labelCache.set(key, {
        tag: "EquationData",
        width: value.width,
        height: value.height,
        rendered: svgCache.get(key)!,
        descent: value.descent,
        ascent: value.ascent,
      });
    }
  });

  return labelCache;
};

export const renderStateToOptRenderState = (
  state: RenderState,
): OptRenderState => ({
  ...state,
  labelCache: labelCacheToOptLabelCache(state.labelCache).optLabelCache,
});

export const optRenderStateToState = (
  state: OptRenderState,
  svgCache: Map<string, HTMLElement>,
) => ({
  ...state,
  labelCache: optLabelCacheToLabelCache(state.labelCache, svgCache),
});

/**
 * Output of label generation.
 */

export type LabelData = EquationData | TextData;
export type EquationData = {
  tag: "EquationData";
} & EquationMeasurement &
  EquationShape;

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
