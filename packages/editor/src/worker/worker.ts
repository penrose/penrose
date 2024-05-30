// one "frame" of optimization is a list of varying numbers
import {
  compileTrio, finalStage,
  insertPending,
  isOptimized,
  LabelMeasurements, nextStage,
  PenroseError,
  PenroseState, resample, runtimeError,
  State, step
} from "@penrose/core";
import {
  CompiledReq,
  LayoutState,
  LayoutStats,
  Req,
  Resp,
  stateToLayoutState,
  WorkerState
} from "./common";
import consola from "consola";

type Frame = number[];

type PartialState = Pick<State, "varyingValues" | "inputs" | "shapes"> & {
  labelCache: LabelMeasurements;
};

// Array of size two. First index is set if main thread wants an update,
// second is set if user wants to send a new trio.
let sharedMemory: Int32Array;
let unoptState: PenroseState;
let optState: PenroseState | null = null;
// the UUID of the current task
let currentTask: string;
let history: Frame[] = [];
let stats: LayoutStats = [];
let workerState: WorkerState = WorkerState.Init;
let shouldFinish = false;

const log = (consola as any)
  .create({ level: (consola as any).LogLevel.Info })
  .withScope("worker:server");

// Wrapper function for postMessage to ensure type safety
const respond = (response: Resp) => {
  postMessage(response);
};

const respondError = (error: PenroseError) => {
  throw error;  // TODO: actually respond as an error
}

onmessage = async ({ data }: MessageEvent<Req>) => {
  const badStateError = () => {
    respondError(runtimeError(`Cannot receive ${data.tag} in worker state ${workerState}`));
  }
  switch (workerState) {
    case WorkerState.Init:
      switch (data.tag) {
        case 'CompiledReq':
          log.info('Received CompiledReq in state Init');
          await compileAndRespond(data);
          break;

        default:
          badStateError();
          break;
      }
      break;

    case WorkerState.Compiled:
      switch (data.tag) {
        case 'OptimizingReq':
          log.info('Received OptimizingReq in state Compiled');
          const stateWithoutLabels: PartialState = unoptState;
          unoptState = {
            ...stateWithoutLabels,
            labelCache: data.labelCache,
          } as PenroseState;
          workerState = WorkerState.Optimizing;
          respondOptimizing();
          optimize(insertPending(unoptState));
          break;

        case 'UpdateReq':
          log.info('Received UpdateReq in state Compiled');
          respondUpdate(stateToLayoutState(optState ?? unoptState), stats);
          break;

        case 'ComputeShapesReq':
          log.info('Received ComputeShapesReq in state Compiled');
          if (data.index >= history.length) {
            respondError(runtimeError(`Index ${data.index} to large for history`));
            break;
          }
          {
            const state = optState ?? unoptState;
            const newShapes: LayoutState = stateToLayoutState({
              ...state,
              varyingValues: history[data.index],
            });
            respondUpdate(newShapes, stats);
          }
          break;

        case 'CompiledReq':
          log.info('Received CompiledReq in state Compiled');
          await compileAndRespond(data);
          break;

        case 'ResampleReq':
          log.info('Received ResampleReq in state Compiled');
          const { variation } = data;
          const resampled = resample({ ...unoptState, variation });
          respondOptimizing();
          optimize(insertPending(resampled));
          break;

        case 'InterruptReq':
          // rare edge case, but can happen if interrupt is requested _just_
          // before optimization finishes
          break;

        default:
          badStateError();
          break;
      }
      break;

    case WorkerState.Optimizing:
      log.info('Received request during optimization');

      if (optState === null) {
        respondError(runtimeError(`OptState was null on request during optimizing`));
        return;
      }

      switch (data.tag) {
        case 'UpdateReq':
          log.info('Received UpdateReq in state Optimizing');
          respondUpdate(stateToLayoutState(optState), stats);
          break;

        case 'ComputeShapesReq':
          log.info('Received ComputeShapesReq in state Optimizing');
          const newShapes: LayoutState = stateToLayoutState({
            ...unoptState,
            varyingValues: history[data.index],
          });
          respondUpdate(newShapes, stats);
          break;

        case 'InterruptReq':
          log.info('Received InterruptReq in state Optimizing');
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
  const {
    domain,
    substance,
    style,
    variation,
    jobId
  } = data;

  // save the id for the current task
  currentTask = jobId;
  const compileResult = await compileTrio({
    domain,
    substance,
    style,
    variation,
  });

  if (compileResult.isErr()) {
    respondError(compileResult.error);
  } else {
    unoptState = compileResult.value;
    workerState = WorkerState.Compiled;
    respondCompiled(jobId, unoptState);
  }
}

const respondInit = () => {
  respond({
    tag: 'InitResp'
  });
}

const respondCompiled = (id: string, state: PenroseState) => {
  respond({
    tag: 'CompiledResp',
    jobId: id,
    shapes: state.shapes
  });
}

const respondOptimizing = () => {
  respond({
    tag: 'OptimizingResp'
  });
}

const respondUpdate = (
  state: LayoutState,
  stats: LayoutStats
) => {
  respond({
    tag: 'UpdateResp',
    state,
    stats,
  })
}

const respondFinished = (state: PenroseState, stats: LayoutStats) => {
  respond({
    tag: 'FinishedResp',
    state: stateToLayoutState(state),
    stats,
  });
}

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
  const numSteps = 1;
  let i = 0;
  const optStep = () => {
    log.info(i);
    let j = 0;
    history.push(state.varyingValues);
    const steppedState =
      step(state, { until: (): boolean => j++ >= numSteps });
    if (steppedState.isErr()) {
      respondError(steppedState.error);
      return;
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
    i++;
  }

  while (!isOptimized(state)) {
    /* queues optStep as next in the event queue, after onmessage if a message
     has been received. await-ing is not enough, since this will queue optStep
     as a _microtask_, which will still run before onmessage */
    await new Promise<void>(resolve => {
      setTimeout(resolve, 0);
    }).then(optStep);

    if (shouldFinish) {
      log.info('Optimization finishing early');
      shouldFinish = false;
      break;
    }
  }

  log.info('Optimization finished');
  workerState = WorkerState.Compiled;
  respondFinished(state, stats);
}

respondInit();