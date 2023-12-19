import {
  OptLabelCache,
  PenroseError,
  PenroseState,
  compileTrio,
  insertPending,
  isOptimized,
  optLabelCacheToLabelCache,
  resample,
  stateToOptRenderState,
  stepTimes,
} from "@penrose/core";
import consola from "consola";
import { Req, Resp } from "./message.js";

const log = (consola as any)
  .create({ level: (consola as any).LogLevel.Info })
  .withScope("worker:worker");

// Array of size two. First index is set if main thread wants an update,
// second bit is set if user wants to send a new trio.
let sharedMemory: Int8Array;
let initialState: PenroseState;
let labels: OptLabelCache;
let svgCache: Map<string, HTMLElement>;
// the UUID of the current task
let currentTask: string;

const respondReqLabelCache = (state: PenroseState) => {
  respond({
    tag: "ReqLabelCache",
    shapes: state.shapes,
    id: currentTask,
  });
};

const respondUpdate = async (state: PenroseState) => {
  respond({
    tag: "Update",
    state: stateToOptRenderState(state),
    id: currentTask,
  });
};

const respondError = (error: PenroseError) => {
  const resp: Resp = { tag: "Error", error, id: currentTask };
  respond(resp);
};

const respondReady = () => {
  respond({ tag: "Ready" });
};

const respondFinished = (state: PenroseState) => {
  respond({
    tag: "Finished",
    state: stateToOptRenderState(state),
    id: currentTask,
  });
};

// Wrapper function for postMessage to ensure type safety
const respond = (response: Resp) => {
  log.debug("Sending response: ", response);
  postMessage(response);
};

const optimize = (state: PenroseState) => {
  while (!isOptimized(state)) {
    const steppedState = stepTimes(state, 25);
    if (steppedState.isErr()) {
      respondError(steppedState.error);
      return;
    }
    // Main thread wants an update
    if (Atomics.exchange(sharedMemory, 0, 0)) {
      respondUpdate(state);
    }
    // Main thread wants to compile something else
    if (Atomics.exchange(sharedMemory, 1, 0)) {
      respondReady();
      return;
    }
    state = steppedState.value;
  }
  respondFinished(state);
};

onmessage = async ({ data }: MessageEvent<Req>) => {
  log.debug("Received message: ", data);
  if (data.tag === "Init") {
    sharedMemory = new Int8Array(data.sharedMemory);
    respond({ tag: "Ready" });
  } else if (data.tag === "Compile") {
    const { domain, substance, style, variation, id } = data;
    // save the id for the current task
    currentTask = id;
    log.debug("start compiling");
    const compileResult = await compileTrio({
      domain,
      substance,
      style,
      variation,
    });
    log.debug("end compiling");

    if (compileResult.isErr()) {
      respondError(compileResult.error);
    } else {
      initialState = compileResult.value;
      respondReqLabelCache(compileResult.value);
    }
  } else if (data.tag === "RespLabelCache") {
    svgCache = new Map<string, HTMLElement>();
    for (const [key, value] of Object.entries(data.labelCache)) {
      svgCache.set(key, value.rendered);
    }
    labels = data.labelCache;
    initialState = {
      ...initialState,
      labelCache: optLabelCacheToLabelCache(data.labelCache, svgCache),
    };
    optimize(insertPending(initialState));
  } else if (data.tag === "Resample") {
    const { variation } = data;
    const resampled = resample({ ...initialState, variation });
    optimize(
      insertPending({
        ...resampled,
        labelCache: optLabelCacheToLabelCache(labels, svgCache),
      }),
    );
    // respondUpdate(resampled);
  } else {
    // Shouldn't ever happen
    console.error(`Unknown request: `, data);
  }
};
