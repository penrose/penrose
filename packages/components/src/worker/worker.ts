import {
  compileTrio,
  PenroseError,
  PenroseState,
  RenderState,
  insertPending,
  stateConverged,
  stepStateSafe,
} from "@penrose/core";
import { Req, Resp } from "./message";

// Array of size two. First index is set if main thread wants an update,
// second bit is set if user wants to send a new trio.
let sharedMemory: Int8Array;
let initialState: PenroseState;

const renderStateFromState = (state: PenroseState) : RenderState => {
  return {
    variation: state.variation,
    labelCache: state.labelCache,
    canvas: state.canvas,
    shapes: state.computeShapes(state.varyingValues),
  }
}

const respondReqLabelCache = (state: PenroseState) => {
  respond({
    tag: "ReqLabelCache",
    shapes: state.shapes,
  })
}

const respondUpdate = async (state: PenroseState) => {
  respond({
    tag: "Update",
    state: renderStateFromState(state),
  });
}

const respondError = (error: PenroseError) =>
  respond({
    tag: "Error",
    error,
  });

const respondReadyForNewTrio = () => {
  respond({ tag: "ReadyForNewTrio" });
};

const respondFinished = (state: PenroseState) => {
  respond({
    tag: "Finished",
    state: renderStateFromState(state),
  });
};

// Wrapper function for postMessage to ensure type safety
const respond = (response: Resp) => {
  postMessage(response)
}

const optimize = (state: PenroseState) => {
  while (!stateConverged(state)) {
    const steppedState = stepStateSafe(state, 25);
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
  if (data.tag === "Init") {
    sharedMemory = new Int8Array(data.sharedMemory);
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
    optimize(insertPending({ ...initialState, labelCache: data.labelCache }));
  } else {
    // Shouldn't ever happen
    console.error(`Unknown request: `, data);
  }
};
