import {
  Canvas,
  LabelCache,
  LabelData,
  LabelMeasurements,
  Num,
  PenroseError,
  Shape,
  State,
} from "@penrose/core";

/** request */

type ID = { id: string };

export type Req =
  | Init
  | ComputeShapes
  | ((Compile | RespLabels | Resample) & ID);

export type Resample = {
  tag: "Resample";
  variation: string;
};

export type ComputeShapes = {
  tag: "ComputeShapes";
  min: number;
  max: number;
  index: number;
};

export type Init = {
  tag: "Init";
  sharedMemory: SharedArrayBuffer;
};

export type RespLabels = {
  tag: "RespLabels";
  labelCache: LabelMeasurements;
};

export type ReqLabels = {
  tag: "ReqLabels";
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
export type Resp = Ready | ((Update | Error | Finished | ReqLabels) & ID);

export type Update = {
  tag: "Update";
  state: LayoutState;
  stats: LayoutStats;
};

export type Finished = {
  tag: "Finished";
  state: LayoutState;
  stats: LayoutStats;
};

export type Error = {
  tag: "Error";
  error: PenroseError;
};

export type Ready = {
  tag: "Ready";
};

// state passed from the worker to the main thread
export interface LayoutState {
  varyingValues: number[];
  variation: string;
  labelCache: LabelMeasurements;
  canvas: Canvas;
  shapes: Shape<number>[];
  optStages: string[];
  currentStageIndex: number;
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
