import { Gradient, OptState } from "@penrose/optimizer";
import { Canvas, InputMeta } from "../shapes/Samplers";
import * as ad from "./ad";
import { A } from "./ast";
import { StyleWarning } from "./errors";
import { Shape, ShapeAD } from "./shape";
import { ConstrFn, ObjFn } from "./style";
import { WithContext } from "./styleSemantics";
import { FloatV } from "./value";

export type ShapeFn = (xs: number[]) => Shape[];

export type OptPipeline = string[];

export type StagedConstraints = Map<
  string,
  {
    inputMask: boolean[];
    objMask: boolean[];
    constrMask: boolean[];
  }
>;

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
  labelCache: OptLabelCache;
  shapes: ShapeAD[];
  canvas: Canvas;
  gradient: Gradient;
  currentStageIndex: number;
  optStages: string[];
  computeShapes: ShapeFn;
}

// State optimizer responds with for main thread to render
export interface OptRenderState {
  variation: string;
  labelCache: OptLabelCache;
  canvas: Canvas;
  shapes: Shape[];
}

// Same as OptRenderState, but includes the rendered label SVG in the label cache.
// Rendered SVG is saved on main thread while worker optimizes since it can't be cloned.
// When worker sends back OptRenderState, it gets converted to RenderState on main thread
export interface RenderState {
  variation: string;
  labelCache: LabelCache;
  canvas: Canvas;
  shapes: Shape[];
}

/**
 * Output of label generation.
 */

export type LabelData = EquationData | TextData;

export type OptLabelData = OptEquationData | TextData;

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

export interface OptEquationData {
  tag: "OptEquationData";
  width: FloatV<number>;
  height: FloatV<number>;
}

export type LabelCache = Map<string, LabelData>;

// A Label Cache but without the rendered HTMLElement because an HTMLElement can't
// be cloned to the worker thread
export type OptLabelCache = Map<string, OptLabelData>;

export type OptStages = "All" | Set<string>;

/**
 * Generic export interface for constraint or objective functions
 */
export interface Fn {
  ast: WithContext<ObjFn<A> | ConstrFn<A>>;
  output: ad.Num;
  optStages: OptStages;
}

export const stateToOptRenderState = (state: State) : OptRenderState => {
  return {
    variation: state.variation,
    labelCache: state.labelCache,
    canvas: state.canvas,
    shapes: state.computeShapes(state.varyingValues),
  }
}

const labelDataToOptLabelData = (labelData: LabelData): OptLabelData => {
  if (labelData.tag === "EquationData") {
    return {
      tag: "OptEquationData",
      width: labelData.width,
      height: labelData.height,
    };
  } 
  return labelData;
}

export const labelCacheToOptLabelCache = (labelCache: LabelCache) => {
  const optLabelCache: OptLabelCache = new Map();
  const svgCache: Map<string, HTMLElement> = new Map();

  labelCache.forEach((value, key) => {
    optLabelCache.set(key, labelDataToOptLabelData(value));
    if (value.tag === "EquationData") {
      svgCache.set(key, value.rendered);
    }
  });

  return { optLabelCache, svgCache }
}

export const optLabelCacheToLabelCache = (optLabelCache: OptLabelCache, svgCache: Map<string, HTMLElement>) => {
  const labelCache: LabelCache = new Map();

  optLabelCache.forEach((value, key) => {
    if (value.tag === "OptEquationData") {
      labelCache.set(key, {
        tag: "EquationData",
        width: value.width,
        height: value.height,
        rendered: svgCache.get("rendered")!,
      })
    }
  });

  return labelCache;
}

export const renderStateToOptRenderState = (state: RenderState): OptRenderState => ({
    ...state,
    labelCache: labelCacheToOptLabelCache(state.labelCache).optLabelCache,
})

export const optRenderStateToState = (state: OptRenderState, svgCache: Map<string, HTMLElement>) => ({
  ...state,
  labelCache: optLabelCacheToLabelCache(state.labelCache, svgCache),
})
