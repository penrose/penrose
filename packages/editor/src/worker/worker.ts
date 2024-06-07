// one "frame" of optimization is a list of varying numbers
import {
  compileTrio,
  err,
  finalStage,
  insertPending,
  isOptimized,
  LabelMeasurements,
  nextStage,
  ok,
  PenroseState,
  resample,
  Result,
  runtimeError,
  State,
  step,
} from "@penrose/core";
import consola from "consola";
import _, { last } from "lodash";
import { Unit } from "true-myth";
import {
  CompiledReq,
  DragShapeReq,
  LayoutState,
  LayoutStats,
  Req,
  Resp,
  stateToLayoutState,
  WorkerState,
} from "./common.js";
import { DragError, WorkerError } from "./errors.js";

type Frame = number[];

type PartialState = Pick<State, "varyingValues" | "inputs" | "shapes"> & {
  labelCache: LabelMeasurements;
};

// state returned by compileTrio
let unoptState: PenroseState;

// most recent state computed through optimization
let optState: PenroseState | null = null;

// the UUID of the current task
let currentTask: string;

let history: Frame[] = [];
let stats: LayoutStats = [];
let workerState: WorkerState = WorkerState.Init;

// set to true to cause optimizer to finish before the next step
let shouldFinish = false;
let dragStart: [number, number] | null = null;

const maxUpdateHz = 50;
let lastUpdateTime = self.performance.now();

const log = (consola as any)
  .create({ level: (consola as any).LogLevel.Warn })
  .withScope("worker:server");

// Wrapper function for postMessage to ensure type safety
const respond = (response: Resp) => {
  postMessage(response);
};

const respondError = (error: WorkerError) => {
  respond({
    tag: "ErrorResp",
    error,
  });
};

self.onmessage = async ({ data }: MessageEvent<Req>) => {
  const badStateError = () => {
    respondError({
      tag: "BadStateError",
      request: data.tag,
      workerState: workerState,
      nextWorkerState: workerState,
    });
  };

  switch (workerState) {
    case WorkerState.Init:
      switch (data.tag) {
        case "CompiledReq":
          log.info("Received CompiledReq in state Init");
          await compileAndRespond(data);
          break;

        default:
          badStateError();
          break;
      }
      break;

    case WorkerState.Compiled:
      switch (data.tag) {
        case "OptimizingReq":
          log.info("Received OptimizingReq in state Compiled");
          const stateWithoutLabels: PartialState = unoptState;
          unoptState = {
            ...stateWithoutLabels,
            labelCache: data.labelCache,
          } as PenroseState;
          optState = _.cloneDeep(unoptState);
          workerState = WorkerState.Optimizing;
          respondOptimizing();
          // launch optimization worker asynchronously
          // we don't await so that we can accept new messages
          optimize(insertPending(optState));
          break;

        case "ComputeShapesReq":
          log.info("Received ComputeShapesReq in state Compiled");
          respondShapes(data.index);
          break;

        case "CompiledReq":
          log.info("Received CompiledReq in state Compiled");
          await compileAndRespond(data);
          break;

        case "ResampleReq":
          log.info("Received ResampleReq in state Compiled");
          const { variation } = data;
          // resample can fail, but doesn't return a result. Hence, try/catch
          try {
            const resampled = _.cloneDeep(
              resample({ ...unoptState, variation }),
            );
            workerState = WorkerState.Optimizing;
            respondOptimizing();
            optimize(insertPending(resampled));
          } catch (err: any) {
            respondError(err);
          }
          break;

        case "DragShapeReq":
          log.info("Received DragShapeReq in state Compiled");
          const draggedState = dragShape(data);
          if (draggedState.isErr()) {
            respondError({
              ...draggedState.error,
              nextWorkerState: workerState,
            });
            break;
          }

          workerState = WorkerState.Optimizing;
          respondOptimizing();
          optimize(insertPending(optState!));
          break;

        case "InterruptReq":
          // rare edge case, but can happen if interrupt is requested _just_
          // after optimization finishes
          break;

        default:
          badStateError();
          break;
      }
      break;

    case WorkerState.Optimizing:
      log.info("Received request during optimization");

      if (optState === null) {
        respondError({
          tag: "FatalWorkerError",
          error: runtimeError("OptState was null during optimization"),
        });
        return;
      }

      switch (data.tag) {
        case "ComputeShapesReq":
          log.info("Received ComputeShapesReq in state Optimizing");
          respondShapes(data.index);
          break;

        case "InterruptReq":
          log.info("Received InterruptReq in state Optimizing");
          shouldFinish = true;
          break;

        case "DragShapeReq":
          log.info("Received DragShapeReq in state Optimizing");
          const draggedState = dragShape(data);
          if (draggedState.isErr()) {
            respondError({
              ...draggedState.error,
              nextWorkerState: workerState,
            });
            break;
          }
          respond({
            tag: "DragOkResp",
          });
          break;

        default:
          badStateError();
          break;
      }
      break;
  }
};

const dragShape = (data: DragShapeReq): Result<Unit, DragError> => {
  if (!optState) {
    return err({
      tag: "DragError",
      nextWorkerState: workerState,
      message: "No opt state on drag",
    });
  }

  if (!optState.draggableShapePaths.has(data.shapePath)) {
    return err({
      tag: "DragError",
      message: "undraggable shape",
      nextWorkerState: workerState,
    });
  }

  const fieldPath = data.shapePath + ".center";
  const center = optState.inputIdxsByPath.get(fieldPath);
  let xIdx, yIdx;
  if (
    center &&
    center.tag === "Val" &&
    (center.contents.tag === "ListV" ||
      center.contents.tag === "VectorV" ||
      center.contents.tag === "TupV")
  ) {
    xIdx = center.contents.contents[0]!;
    yIdx = center.contents.contents[1]!;
  } else {
    return err({
      tag: "DragError",
      message: `could not find center indices at path ${fieldPath}`,
      nextWorkerState: workerState,
    });
  }

  if (dragStart == null) {
    dragStart = [optState.varyingValues[xIdx], optState.varyingValues[yIdx]];

    const previouslyPinned = new Set(optState.pinnedInputIdxs);
    optState.pinnedInputIdxs = new Set([xIdx, yIdx]);

    for (const [stage, masks] of optState.constraintSets) {
      // for (const pinnedInput of previouslyPinned) {
      //   masks.inputMask[pinnedInput] = true;
      // }

      for (const pinnedInput of optState.pinnedInputIdxs) {
        masks.inputMask[pinnedInput] = false;
      }
    }
  }

  optState.varyingValues[xIdx] = dragStart[0] + data.dx;
  optState.varyingValues[yIdx] = dragStart[1] + data.dy;
  optState.currentStageIndex = 0;

  optState.params = _.cloneDeep(unoptState.params);

  if (data.finish) {
    dragStart = null;
  }

  return ok();
};

const compileAndRespond = async (data: CompiledReq) => {
  const { domain, substance, style, variation, jobId } = data;

  const compileResult = await compileTrio({
    domain,
    substance,
    style,
    variation,
  });

  if (compileResult.isErr()) {
    respondError({
      tag: "CompileError",
      error: compileResult.error,
      nextWorkerState: workerState,
    });
  } else {
    // save the id for the current task
    currentTask = jobId;
    unoptState = compileResult.value;
    workerState = WorkerState.Compiled;
    respondCompiled(jobId, unoptState);
  }
};

const respondShapes = (index: number) => {
  if (index >= history.length) {
    respondError({
      tag: "HistoryIndexOutOfRangeError",
      index: index,
      historyLength: history.length,
      nextWorkerState: workerState,
    });
    return;
  }
  {
    const state = optState ?? unoptState;
    const newShapes: LayoutState = stateToLayoutState({
      ...state,
      varyingValues: history[index],
    });
    respond({
      tag: "ComputeShapesResp",
      state: newShapes
    })
  }
};

const respondInit = () => {
  respond({
    tag: "InitResp",
  });
};

const respondCompiled = (id: string, state: PenroseState) => {
  respond({
    tag: "CompiledResp",
    jobId: id,
    shapes: state.shapes,
    warnings: state.warnings,
  });
};

const respondOptimizing = () => {
  respond({
    tag: "OptimizingResp",
  });
};

const respondUpdate = (state: LayoutState, stats: LayoutStats) => {
  respond({
    tag: "UpdateResp",
    state,
    stats,
  });
};

const respondFinished = (state: PenroseState, stats: LayoutStats) => {
  respond({
    tag: "FinishedResp",
    state: stateToLayoutState(state),
    stats,
  });
};

const optimize = async (state: PenroseState) => {
  optState = state;
  // lastUpdateTime = self.performance.now();

  // reset history and stats per optimization run
  // TODO: actually return them?
  history = [];
  stats = [
    {
      name: state.optStages.length === 1 ? "default" : state.optStages[0],
      steps: 0,
    },
  ];

  const numStepsPerHistory = 1;
  let i = 0;

  // on message, we will take a step, but these messages will be queued behind
  // self.onmessage, so we can interrupt optimization with a message
  const optStepMsgChannel = new MessageChannel();

  // take one optimization step
  const optStep = () => {
    try {
      if (shouldFinish) {
        // set by onmessage if we need to stop
        log.info("Optimization finishing early");
        shouldFinish = false;
        workerState = WorkerState.Compiled;
        respondFinished(state, stats);
        return;
      }

      log.info(i);
      let j = 0;
      history.push(state.varyingValues);
      const steppedState = step(state, {
        until: (): boolean => j++ >= numStepsPerHistory,
      });
      if (steppedState.isErr()) {
        throw steppedState.error;
      } else {
        const stepped = steppedState.value;
        if (isOptimized(stepped) && !finalStage(stepped)) {
          const nextInitState = nextStage(stepped);
          state = nextInitState;
          const currentStage = state.optStages[state.currentStageIndex];
          // add the total steps taken by the previous stage
          stats.push({
            name: currentStage,
            steps: 0,
          });
          i = 0;
        } else {
          state = stepped;
          // update the step count for the current stage
          stats[stats.length - 1].steps = i;
        }
      }

      optState = state;

      if (isOptimized(state)) {
        log.info("Optimization finished");
        workerState = WorkerState.Compiled;
        respondFinished(state, stats);
        return;
      }

      const now = self.performance.now();
      if ((now - lastUpdateTime) >= 1000. / maxUpdateHz) {
        respondUpdate(stateToLayoutState(optState), stats);
        lastUpdateTime = now;
      }

      i++;
      optStepMsgChannel.port2.postMessage(null);
    } catch (err: any) {
      log.info("Optimization failed. Quitting without finishing...");
      workerState = WorkerState.Compiled;
      optState = null;
      respondError({
        tag: "OptimizationError",
        error: err,
        nextWorkerState: WorkerState.Compiled,
      });
      return;
    }
  };

  optStepMsgChannel.port1.onmessage = () => {
    optStep();
  };
  optStepMsgChannel.port2.postMessage(null);
};

// tell OptimizerWorker that we're ready to go
respondInit();
