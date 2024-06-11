// one "frame" of optimization is a list of varying numbers
import {
  compileTrio,
  finalStage,
  insertPending,
  isOptimized,
  LabelMeasurements,
  nextStage,
  PenroseState,
  resample,
  runtimeError,
  State,
  step,
} from "@penrose/core";
import consola from "consola";
import {
  CompiledReq,
  LayoutState,
  LayoutStats,
  Req,
  Resp,
  stateToLayoutState,
  WorkerState,
} from "./common.js";
import { WorkerError } from "./errors.js";

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
          workerState = WorkerState.Optimizing;
          respondOptimizing();
          // launch optimization worker asynchronously
          // we don't await so that we can accept new messages
          optimize(insertPending(unoptState));
          break;

        case "UpdateReq":
          log.info("Received UpdateReq in state Compiled");
          respondUpdate(stateToLayoutState(optState ?? unoptState), stats);
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
            const resampled = resample({ ...unoptState, variation });
            workerState = WorkerState.Optimizing;
            respondOptimizing();
            optimize(insertPending(resampled));
          } catch (err: any) {
            respondError(err);
          }
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
        case "UpdateReq":
          log.info("Received UpdateReq in state Optimizing");
          respondUpdate(stateToLayoutState(optState), stats);
          break;

        case "ComputeShapesReq":
          log.info("Received ComputeShapesReq in state Optimizing");
          respondShapes(data.index);
          break;

        case "InterruptReq":
          log.info("Received InterruptReq in state Optimizing");
          shouldFinish = true;
          break;

        default:
          badStateError();
          break;
      }
      break;
  }
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
    respondUpdate(newShapes, stats);
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
