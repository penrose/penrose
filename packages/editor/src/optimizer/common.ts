import {
  Canvas,
  collectLabels,
  InteractivityInfo,
  isPenroseError,
  LabelCache,
  LabelData,
  LabelMeasurements,
  mathjaxInit,
  Num,
  PenroseError,
  PenroseWarning,
  Shape,
  showError,
  State,
} from "@penrose/core";
import consola from "consola";
import { pick } from "lodash";
import { Result } from "true-myth";
import { Interaction } from "../utils/interactionUtils.js";

// Config

export const logLevel = (consola as any).LogLevel.Warn;

// Basic types

/** Unique ID assigned to every request/response pair */
export type MessageID = number;

/**
 * Unique ID assigned to every new worker, which each handle one diagrams
 * compilation, optimization, interaction, etc.
 */
export type DiagramID = number;

/**
 * Unique ID assigned to every sequence of optimizer steps. Only necessarily
 * unique within a diagram.
 */
export type StepSequenceID = number;

/**
 * History is a DAG of `StepSequences`, which are a series of optimizer
 * steps, stages, and states. Each of these layout sequences has an id, and
 * the steps are indexed by order.
 */
export type HistoryLoc = {
  sequenceId: StepSequenceID;
  frame: number;
};

/**
 * Array of stage name and optimizer steps per stage.
 */
export type LayoutStats = {
  name: string;
  frames: number;
  cumulativeFrames: number;
}[];

/**
 * Current state of a sequence of optimizer states:
 *  - `"Pending"`: optimizer has not finished (ongoing, interrupted). Could
 *    or will continue.
 *  - `"Done"`: optimizer finished normally
 *  - `OptimizerError`: optimizer errored and cannot continue.
 */
export type StepSequenceState =
  | { tag: "Pending" }
  | { tag: "Done" }
  | OptimizationError;

/**
 * Info about a step sequence.
 *  - `LayoutStats`: stage names and steps
 *  - `parent`: The step sequence and step from which this sequence originated,
 *    or null if this is a root sequence
 *  - `state`: State of the step sequence
 *  - `variation`: Variation string on last resample
 */
export type StepSequenceInfo = {
  layoutStats: LayoutStats;
  parent: HistoryLoc | null;
  child: StepSequenceID | null;
  state: StepSequenceState;
  variation: string;
  pinnedInputPaths: Map<string, Set<number>>;
};

/**
 * Map from `StepSequenceID`s to `StepSequenceInfo`s
 */
export type HistoryInfo = Map<StepSequenceID, StepSequenceInfo>;

/** Type of a resolve for a promise waiting on a `MessageResult` */
export type MessageResolve = (result: MessageResult) => void;

export type PartialInteractivityInfo = Pick<
  InteractivityInfo,
  "translatableShapePaths" | "scalableShapePaths" | "draggingConstraints"
>;

/**
 * A `RenderState`, but without rendered labels which cannot be sent across
 * threads. Use `layoutStateToRenderState` to combine with an `svgCache` to
 * generate a `RenderState` for rendering.
 */
export type LayoutState = {
  canvas: Canvas;
  shapes: Shape<number>[];
  labelMeasurements: LabelMeasurements;
  variation: string;
  interactivityInfo: PartialInteractivityInfo;
};

/** Minimal state needed for rendering */
export type RenderState = {
  canvas: Canvas;
  shapes: Shape<number>[];
  labelCache: LabelCache;
  variation: string;
  interactivityInfo: PartialInteractivityInfo;
};

/** Info for a successful compile */
export type CompileInfo = {
  diagramId: DiagramID;
  warnings: PenroseWarning[];
};

export type InteractionInfo = {
  sequenceId: StepSequenceID;
  // performance optimization: we pass back the last history so we can immediately
  // update the stored info without a poll
  historyInfo: HistoryInfo;
};

// Message types

/** Requests sent from main thread to broker or broker to worker */
export type MessageRequest = {
  tag: "MessageRequest";
  messageId: MessageID;
  data: MessageRequestData;
};

/** Responses to `MessageRequest`s */
export type MessageResponse = {
  tag: "MessageResponse";
  messageId: MessageID;
  result: MessageResult;
};

/** Unprompted notifications between threads. No response can be sent. */
export type Notification = {
  tag: "Notification";
  data: NotificationData;
};

/** Data sent in a `MessageRequest` */
export type MessageRequestData =
  | CompileRequestData
  | PollRequestData
  | ComputeLayoutRequestData
  | ResampleRequestData
  | InteractionRequestData;

/** Result type contained in a `MessageResponse` */
export type MessageResult =
  | CompileResult
  | PollResult
  | ComputeLayoutResult
  | ResampleResult
  | InteractionResult;

/** Data contained in a `Notification` */
export type NotificationData =
  | InitData
  | LabelMeasurementData
  | DiscardDiagramData;

/** Ensure request data and result pairs have the same tag, to check validity */
export enum MessageTags {
  Compile = "Compile",
  Poll = "Poll",
  ComputeLayout = "ComputeLayout",
  DiscardDiagram = "DiscardDiagram",
  Init = "Init",
  LabelMeasurements = "LabelMeasurements",
  Resample = "Resample",
  Interaction = "Interaction",
}

// Request Data

export type CompileRequestData = {
  tag: MessageTags.Compile;
  domain: string;
  style: string;
  substance: string;
  variation: string;
};

export type PollRequestData = {
  tag: MessageTags.Poll;
  diagramId: number;
};

export type ComputeLayoutRequestData = {
  tag: MessageTags.ComputeLayout;
  diagramId: DiagramID;
  historyLoc: HistoryLoc;
};

export type ResampleRequestData = {
  tag: MessageTags.Resample;
  diagramId: DiagramID;
  variation: string;
};

export type InteractionRequestData = {
  tag: MessageTags.Interaction;
  diagramId: DiagramID;
  parentHistoryLoc: HistoryLoc;
  interaction: Interaction;
};

// Notification Data

export type InitData = {
  tag: MessageTags.Init;
};

export type LabelMeasurementData = {
  tag: MessageTags.LabelMeasurements;
  diagramId: DiagramID;
  labelMeasurements: LabelMeasurements;
};

export type DiscardDiagramData = {
  tag: MessageTags.DiscardDiagram;
  diagramId: DiagramID;
};

// Results

export type TaggedOk<V, T> = {
  tag: T;
  variant: "Ok";
  value: V;
};

export type TaggedErr<E, T> = {
  tag: T;
  variant: "Err";
  error: E;
};

/**
 * A Result type, but with a tag. Supertype of `MessageResult`.
 */
export type TaggedResult<V, E, T> = TaggedOk<V, T> | TaggedErr<E, T>;

export type CompileResult = TaggedResult<
  CompileInfo,
  PenroseError,
  MessageTags.Compile
>;

export type PollResult = TaggedResult<
  HistoryInfo,
  InvalidDiagramIDError,
  MessageTags.Poll
>;

export type ComputeLayoutResult = TaggedResult<
  LayoutState,
  InvalidDiagramIDError | InvalidHistoryLocError | PenroseError,
  MessageTags.ComputeLayout
>;

export type ResampleResult = TaggedResult<
  StepSequenceID,
  InvalidDiagramIDError,
  MessageTags.Resample
>;

export type InteractionResult = TaggedResult<
  InteractionInfo,
  InvalidDiagramIDError | InvalidHistoryLocError | InteractionError,
  MessageTags.Interaction
>;

// Errors

export type InvalidDiagramIDError = {
  tag: "InvalidDiagramIDError";
  id: DiagramID;
  validIds: Set<DiagramID>;
};

export type InvalidHistoryLocError = {
  tag: "InvalidHistoryLocError";
  historyLoc: HistoryLoc;
  historyInfo: HistoryInfo;
};
export type OptimizationError = {
  tag: "OptimizationError";
  error: unknown;
};

export type InteractionError = {
  tag: "InteractionError";
  message: string;
};

export type OptimizerError =
  | OptimizationError
  | InvalidDiagramIDError
  | InvalidHistoryLocError
  | InteractionError;

// Utils

class NumericIDGenerator {
  private nextId = 0;

  next = () => {
    return this.nextId++;
  };
}

// We reexport as different generators for each ID type, in case we decide
// not to use numbers in the future
export {
  NumericIDGenerator as DiagramIDGenerator,
  NumericIDGenerator as MessageIDGenerator,
  NumericIDGenerator as StepSequenceIDGenerator,
};

export type Tagged<T> = {
  tag: T;
};

/**
 * Make a request to a worker.
 *
 * @param worker Worker to request
 * @param data Request data
 * @param resolvesById Map from `MessageID` to `MessageResolve`. Mutated.
 * @param messageIdGenerator Generator from which we should get the next message id
 */
export const request = <T, R extends MessageResult & Tagged<T>>(
  worker: Worker,
  data: MessageRequestData & Tagged<T>,
  resolvesById: Map<MessageID, MessageResolve>,
  messageIdGenerator: NumericIDGenerator,
): Promise<R> => {
  return new Promise((resolve) => {
    const messageId: MessageID = messageIdGenerator.next();
    const request: MessageRequest = {
      tag: "MessageRequest",
      messageId,
      data,
    };
    resolvesById.set(messageId, (result: MessageResult) => {
      if (result.tag === data.tag) {
        (resolve as any)(result);
        resolvesById.delete(messageId);
      } else {
        throw new Error(
          `MessageID ${messageId} expected result of type 
          ${data.tag} but received ${result.tag}`,
        );
      }
    });
    worker.postMessage(request);
  });
};

/**
 * Launch worker, and wait for a notification `InitData` before resolving.
 * @param worker
 */
export const launchAndWaitForInit = async (worker: Worker): Promise<Worker> => {
  return new Promise((resolve) => {
    worker.onmessage = ({ data }: MessageEvent<Notification>) => {
      switch (data.data.tag) {
        case MessageTags.Init:
          worker.onmessage = null;
          resolve(worker);
      }
    };
  });
};

/**
 * Send a `Notification` from a worker to the parent thread.
 * @param data Notification data
 */
export const notify = (data: NotificationData) => {
  const notification: Notification = {
    tag: "Notification",
    data,
  };
  self.postMessage(notification);
};

/**
 * Send a `Notification` to a worker
 * @param worker Worker to notify
 * @param data Notification data
 */
export const notifyWorker = (worker: Worker, data: NotificationData) => {
  const notification: Notification = {
    tag: "Notification",
    data,
  };
  worker.postMessage(notification);
};

/**
 * Respond to the parent thread.
 * @param messageId `MessageID` of the associated request
 * @param result Result of the message
 */
export const respond = (messageId: MessageID, result: MessageResult) => {
  const response = {
    tag: "MessageResponse",
    messageId,
    result,
  };
  self.postMessage(response);
};

/**
 * On receiving a response, call the associated `resolve` and remove the resolve.
 * @param response Message response
 * @param resolvesById Map from `MessageID`s to `MessageResolve`s. Mutated.
 */
export const resolveResponse = (
  response: MessageResponse,
  resolvesById: Map<MessageID, MessageResolve>,
) => {
  const resolve = resolvesById.get(response.messageId);
  if (!resolve) {
    throw new Error(
      `Received response from unknown message id ${response.messageId}`,
    );
  }
  resolve(response.result);
  resolvesById.delete(response.messageId);
};

export const taggedOk = <V, E, T>(tag: T, value: V): TaggedResult<V, E, T> => {
  return {
    tag,
    variant: "Ok",
    value,
  };
};

export const taggedErr = <V, E, T>(tag: T, error: E): TaggedResult<V, E, T> => {
  return {
    tag,
    variant: "Err",
    error,
  };
};

export const isOk = <V, E, T>(
  result: TaggedResult<V, E, T>,
): result is TaggedOk<V, T> => {
  return result.variant === "Ok";
};

export const isErr = <V, E, T>(
  result: TaggedResult<V, E, T>,
): result is TaggedErr<E, T> => {
  return result.variant === "Err";
};

/**
 * Strip `labelData` of all rendered (and non-message-able) fields
 * @param labelData
 */
export const removeRenderedLabels = <T extends LabelData>(
  labelData: T,
): LabelData => {
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

/**
 * Convert an `OptimizerError` to a readable string.
 * @param error Error to show
 */
export const showOptimizerError = (error: OptimizerError): string => {
  switch (error.tag) {
    case "OptimizationError":
      const prefix = "OptimizationError: ";
      if (error.error instanceof Object && "message" in error.error) {
        return prefix + error.error.message;
      } else if (isPenroseError(error.error)) {
        return prefix + showError(error.error);
      } else {
        console.error("Unknown optimization error: ", error.error);
        return prefix + "unknown error. Check console.";
      }

    case "InvalidDiagramIDError":
      return `InvalidDiagramIDError: Id ${error.id} is invalid. Valid ids: ${[
        ...error.validIds.keys(),
      ]}`;

    case "InvalidHistoryLocError":
      return `InvalidHistoryLocError:\n    Location: ${JSON.stringify(
        error.historyLoc,
      )}\n    History Info: ${JSON.stringify([
        ...error.historyInfo.entries(),
      ])}\n`;

    case "InteractionError":
      return `InteractionError: ${error.message}`;
  }
};

/**
 * Combine label measurements and svg cache to create rendered labels.
 * @param optLabelCache Label measurements
 * @param svgCache SVG cache
 */
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

/**
 * Separate labels into measurements and svgs
 * @param labelCache
 */
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

export const layoutStateToRenderState = (
  layoutState: LayoutState,
  svgCache: Map<string, HTMLElement>,
) => ({
  ...layoutState,
  labelCache: addRenderedLabels(layoutState.labelMeasurements, svgCache),
});

export const collectAndSeparateLabels = async (
  shapes: Shape<Num>[],
): Promise<
  Result<
    { optLabelCache: LabelMeasurements; svgCache: Map<string, HTMLElement> },
    PenroseError
  >
> => {
  const convert = mathjaxInit();
  const labelCache = await collectLabels(shapes, convert);
  if (labelCache.isErr()) {
    return Result.err(labelCache.error);
  }

  return Result.ok(separateRenderedLabels(labelCache.value));
};

export const getPartialInteractivityInfo = (
  interactivityInfo: InteractivityInfo,
): PartialInteractivityInfo => {
  return pick(interactivityInfo, [
    "translatableShapePaths",
    "scalableShapePaths",
    "draggingConstraints",
  ]);
};

export const stateToLayoutState = (state: State): LayoutState => {
  return {
    variation: state.variation,
    labelMeasurements: separateRenderedLabels(state.labelCache).optLabelCache,
    canvas: state.canvas,
    shapes: state.computeShapes(state.varyingValues),
    interactivityInfo: getPartialInteractivityInfo(state.interactivityInfo),
  };
};

/**
 * A `Map`, but after size grows past `maxLen`, old elements are deleted.
 */
export class SizeBoundedMap<K, V> {
  private readonly _map = new Map<K, V>();
  private readonly maxLen;
  private readonly onDelete;

  private _keys: K[] = [];

  constructor(maxLen: number, onDelete?: (key: K, val: V) => void) {
    this.maxLen = maxLen;
    this.onDelete = onDelete;
  }

  get = (key: K): V | undefined => {
    return this._map.get(key);
  };

  set = (key: K, value: V): void => {
    // if we're about to grow past our maximum length, find the oldest key
    // and remove it from _keys and _map.
    if (this._map.size === this.maxLen) {
      const toDeleteKey = this._keys.shift();
      if (toDeleteKey !== undefined) {
        const toDeleteValue = this._map.get(toDeleteKey)!;
        this._map.delete(toDeleteKey);
        this.onDelete?.(toDeleteKey, toDeleteValue);
      }
    }
    this._keys.push(key);
    this._map.set(key, value);
  };

  delete = (key: K): void => {
    const index = this._keys.findIndex((k) => k === key);
    if (index >= 0) {
      this._keys.splice(index, 1);
      this._map.delete(key);
    }
  };

  keys = () => {
    return [...this._keys];
  };

  map = () => {
    return new Map(this._map);
  };
}
