import {
  Canvas,
  IdxsByPath,
  LabelCache,
  LabelData,
  LabelMeasurements,
  Num,
  PenroseWarning,
  Shape,
  State,
} from "@penrose/core";
import { Interaction } from "../utils/interactionUtils";
import { WorkerError } from "./errors.js";

export enum WorkerState {
  Init = "Init",
  Compiled = "Compiled",
  Optimizing = "Optimizing",
  Error = "Error",
}

export type InitResp = {
  tag: "InitResp";
};

export type CompiledResp = {
  tag: "CompiledResp";
  jobId: string;
  warnings: PenroseWarning[];
  shapes: Shape<Num>[];
};

export type OptimizingResp = {
  tag: "OptimizingResp";
};

export type FinishedResp = {
  tag: "FinishedResp";
  state: LayoutState;
  stats: LayoutStats;
};

export type UpdateResp = {
  tag: "UpdateResp";
  state: LayoutState;
  stats: LayoutStats;
};

export type InteractOkResp = {
  tag: "InteractOkResp";
};

export type ComputeShapesResp = {
  tag: "ComputeShapesResp";
  state: LayoutState;
};

export type ErrorResp = {
  tag: "ErrorResp";
  error: WorkerError;
};

export type Resp =
  | InitResp
  | CompiledResp
  | OptimizingResp
  | FinishedResp
  | UpdateResp
  | ErrorResp
  | InteractOkResp
  | ComputeShapesResp;

export type CompiledReq = {
  tag: "CompiledReq";
  domain: string;
  style: string;
  substance: string;
  variation: string;
  jobId: string;
};

export type OptimizingReq = {
  tag: "OptimizingReq";
  labelCache: LabelMeasurements;
};

export type ResampleReq = {
  tag: "ResampleReq";
  variation: string;
  jobId: string;
};

export type ComputeShapesReq = {
  tag: "ComputeShapesReq";
  index: number;
};

export type InteractReq = {
  tag: "InteractReq";
  interaction: Interaction;
  finish: boolean;
};

export type InterruptReq = {
  tag: "InterruptReq";
};

export type Req =
  | CompiledReq
  | OptimizingReq
  | ResampleReq
  | ComputeShapesReq
  | InterruptReq
  | InteractReq;

// state passed from the worker to the main thread.
// NOTE: there is no DOM element or functions in this state because they cannot be transferred between threads.
export interface LayoutState {
  varyingValues: number[];
  variation: string;
  labelCache: LabelMeasurements;
  canvas: Canvas;
  shapes: Shape<number>[];
  optStages: string[];
  currentStageIndex: number;
  inputIdxsByPath: IdxsByPath;
  translatableShapePaths: Set<string>;
  scalableShapePaths: Set<string>;
  shapesByPath: Map<string, Shape<unknown>>;
}

// state maintained by the main thread for rendering
export interface RenderState {
  variation: string;
  labelCache: LabelCache;
  canvas: Canvas;
  shapes: Shape<number>[];
  varyingValues: number[];
  optStages: string[];
  currentStageIndex: number;
  inputIdxsByPath: IdxsByPath;
  translatableShapePaths: Set<string>;
  scalableShapePaths: Set<string>;
  shapesByPath: Map<string, Shape<unknown>>;
}

// translate from the entire state to the state that is passed to the main thread
export const stateToLayoutState = (state: State): LayoutState => {
  return {
    variation: state.variation,
    labelCache: separateRenderedLabels(state.labelCache).optLabelCache,
    canvas: state.canvas,
    shapes: state.computeShapes(state.varyingValues),
    varyingValues: state.varyingValues,
    optStages: state.optStages,
    currentStageIndex: state.currentStageIndex,
    inputIdxsByPath: state.inputIdxsByPath,
    translatableShapePaths: state.translatableShapePaths,
    scalableShapePaths: state.scalableShapePaths,
    shapesByPath: state.shapesByPath,
  };
};

const removeRenderedLabels = <T extends LabelData>(labelData: T): LabelData => {
  if (labelData.tag === "EquationData") {
    return {
      tag: "EquationData",
      width: labelData.width,
      height: labelData.height,
      descent: labelData.descent,
      ascent: labelData.ascent,
    };
  } else {
    return labelData;
  }
};

export const separateRenderedLabels = (
  labelCache: LabelCache,
): { optLabelCache: LabelMeasurements; svgCache: Map<string, HTMLElement> } => {
  const optLabelCache: LabelMeasurements = new Map<string, LabelData>();
  const svgCache: Map<string, HTMLElement> = new Map();

  labelCache.forEach((value, key) => {
    optLabelCache.set(key, removeRenderedLabels(value));
    if (value.tag === "EquationData") {
      svgCache.set(key, value.rendered);
    }
  });

  return { optLabelCache, svgCache };
};

export const addRenderedLabels = (
  optLabelCache: LabelMeasurements,
  svgCache: Map<string, HTMLElement>,
): LabelCache => {
  const labelCache: LabelCache = new Map();

  optLabelCache.forEach((value, key) => {
    if (value.tag === "EquationData") {
      labelCache.set(key, {
        tag: "EquationData",
        width: value.width,
        height: value.height,
        rendered: svgCache.get(key)!,
        descent: value.descent,
        ascent: value.ascent,
      });
    } else {
      labelCache.set(key, value);
    }
  });
  return labelCache;
};

export const renderStateToLayoutState = (state: RenderState): LayoutState => ({
  ...state,
  labelCache: separateRenderedLabels(state.labelCache).optLabelCache,
});

export const layoutStateToRenderState = (
  state: LayoutState,
  svgCache: Map<string, HTMLElement>,
) => ({
  ...state,
  labelCache: addRenderedLabels(state.labelCache, svgCache),
});

export type LayoutStats = {
  name: string;
  steps: number;
}[];
