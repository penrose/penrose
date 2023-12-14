import {
  PenroseError,
  PenroseState,
  compileTrio,
  insertPending,
  isOptimized,
  optLabelCacheToLabelCache,
  stateToOptRenderState,
  stepTimes,
} from "@penrose/core";
import { Req, Resp } from "./message.js";

// Array of size two. First index is set if main thread wants an update,
// second bit is set if user wants to send a new trio.
let sharedMemory: Int8Array;
let initialState: PenroseState;

const respondReqLabelCache = (state: PenroseState) => {
  respond({
    tag: "ReqLabelCache",
    shapes: state.shapes,
  });
};

const respondUpdate = async (state: PenroseState) => {
  respond({
    tag: "Update",
    state: stateToOptRenderState(state),
  });
};

const respondError = (error: PenroseError) => {
  const resp: Resp = { tag: "Error", error };
  respond(resp);
};

const respondReadyForNewTrio = () => {
  respond({ tag: "ReadyForNewTrio" });
};

const respondFinished = (state: PenroseState) => {
  respond({
    tag: "Finished",
    state: stateToOptRenderState(state),
  });
};

// Wrapper function for postMessage to ensure type safety
const respond = (response: Resp) => {
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
      respondReadyForNewTrio();
      return;
    }
    state = steppedState.value;
  }
  respondFinished(state);
};

onmessage = async ({ data }: MessageEvent<Req>) => {
  console.log("Received message: ", data);

  if (data.tag === "Init") {
    sharedMemory = new Int8Array(data.sharedMemory);
    respond({ tag: "ReadyForNewTrio" });
  } else if (data.tag === "Compile") {
    const { domain, substance, style, variation } = data;
    const compileResult = await compileTrio({
      domain,
      substance,
      style,
      variation,
    });

    if (compileResult.isErr()) {
      respondError(compileResult.error);
    } else {
      initialState = compileResult.value;
      respondReqLabelCache(compileResult.value);
    }
  } else if (data.tag === "RespLabelCache") {
    const svgCache = new Map<string, HTMLElement>();
    for (const [key, value] of Object.entries(data.labelCache)) {
      svgCache.set(key, value.rendered);
    }

    optimize(
      insertPending({
        ...initialState,
        labelCache: optLabelCacheToLabelCache(data.labelCache, svgCache),
      }),
    );
  } else {
    // Shouldn't ever happen
    console.error(`Unknown request: `, data);
  }
};

console.log("worker loaded");
