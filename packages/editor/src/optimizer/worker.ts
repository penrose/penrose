import {
  LabelMeasurements,
  PenroseState,
  compileTrio,
  finalStage,
  insertPending,
  isOptimized,
  nextStage,
  resample as penroseResample,
  start,
  step,
} from "@penrose/core";
import consola from "consola";
import { Result } from "true-myth";
import {
  CompileRequestData,
  CompileResult,
  ComputeLayoutRequestData,
  ComputeLayoutResult,
  HistoryLoc,
  MessageRequest,
  MessageResult,
  MessageTags,
  Notification,
  PollResult,
  ResampleResult,
  SizeBoundedMap,
  StepSequenceID,
  StepSequenceIDGenerator,
  StepSequenceInfo,
  logLevel,
  notify,
  respond,
  stateToLayoutState,
  taggedErr,
  taggedOk,
} from "./common.js";

const log = consola.create({ level: logLevel }).withScope("optimizer:worker");
const stepSequenceIdGenerator = new StepSequenceIDGenerator();
const numStepSequencesToKeep = 3;

let historyInfo = new SizeBoundedMap<StepSequenceID, StepSequenceInfo>(
  numStepSequencesToKeep,
);
/**
 * Varying values sequence for each step sequence. Kept separate from history
 * info so we don't need to recompute history info on every poll.
 */
let historyValues: Map<StepSequenceID, number[][]> = new Map();

/**
 * Context for optimizer. Varying values only represent the last
 * values the optimizer saw; refer to `historyValue` to get the appropriate list
 * for a particular `historyLoc`.
 */
let penroseState: PenroseState | null = null;
let labelMeasurements: LabelMeasurements = new Map();
let optimizerShouldStop = false;

const getParentVaryingValues = (stepSequenceInfo: StepSequenceInfo) => {
  const parentLoc = stepSequenceInfo.parent;
  return parentLoc === null
    ? penroseState!.varyingValues
    : historyValues.get(parentLoc.sequenceId)![parentLoc.step];
};

const compile = async (data: CompileRequestData): Promise<CompileResult> => {
  const compiledState = await compileTrio(data);

  let result: CompileResult;
  if (Result.isOk(compiledState)) {
    log.info("Compiled");
    penroseState = compiledState.value;
    makeNewStepSequence(null, data.variation);
    // diagramId is assigned by broker, not us, so we just pass 0
    result = taggedOk(MessageTags.Compile, {
      diagramId: 0,
      warnings: compiledState.value.warnings,
    });
  } else {
    log.info("Compilation failed");
    result = taggedErr(MessageTags.Compile, compiledState.error);
  }

  return result;
};

const poll = async (): Promise<PollResult> => {
  return taggedOk(MessageTags.Poll, historyInfo.map());
};

const computeLayout = async (
  data: ComputeLayoutRequestData,
): Promise<ComputeLayoutResult> => {
  const varyingValues = historyValues.get(data.historyLoc.sequenceId);
  const stepSequenceInfo = historyInfo.get(data.historyLoc.sequenceId);

  let result: ComputeLayoutResult;
  if (
    varyingValues &&
    stepSequenceInfo &&
    0 <= data.historyLoc.step &&
    data.historyLoc.step < varyingValues.length
  ) {
    const xs = varyingValues[data.historyLoc.step];
    result = taggedOk(
      MessageTags.ComputeLayout,
      stateToLayoutState({
        ...penroseState!,
        varyingValues: xs,
      }),
    );
  } else {
    result = taggedErr(MessageTags.ComputeLayout, {
      tag: "InvalidHistoryLocError",
      historyLoc: data.historyLoc,
      historyInfo: historyInfo.map(),
    });
  }

  return result;
};

const makeNewStepSequence = (
  parentLoc: HistoryLoc | null,
  variation: string,
): StepSequenceID => {
  const id: StepSequenceID = stepSequenceIdGenerator.next();

  const info: StepSequenceInfo = {
    layoutStats: [
      {
        name:
          penroseState!.optStages.length === 1
            ? "default"
            : penroseState!.optStages[0],
        // steps, as it is used in the rest of the codebase, indicates the number
        // of varying values in a list--not the number of optimizer steps. So
        // we initialize this to one, since one varying value is added initially
        steps: 1,
        cumulativeSteps: 1,
      },
    ],
    parent: parentLoc,
    state: "Pending",
    variation,
  };
  const varyingValuesSequence: number[][] = [getParentVaryingValues(info)];

  historyInfo.set(id, info);
  historyValues.set(id, varyingValuesSequence);
  return id;
};

const startOptimize = async (stepSequenceId: StepSequenceID) => {
  const stepSequenceInfo = historyInfo.get(stepSequenceId);
  if (!stepSequenceInfo) {
    throw new Error("invalid step sequence for optimizer");
  }

  const varyingValuesSequence = historyValues.get(stepSequenceId);
  if (!varyingValuesSequence) {
    stepSequenceInfo.state = {
      tag: "OptimizationError",
      error: new Error(
        `No varying values found for step sequence id ${stepSequenceId}`,
      ),
    };
    return;
  }

  const initVaryingValues = getParentVaryingValues(stepSequenceInfo);
  penroseState = {
    ...penroseState!,
    varyingValues: [...initVaryingValues],
    params: start(initVaryingValues.length),
    currentStageIndex: stepSequenceInfo.layoutStats.length - 1,
  };

  penroseState = insertPending(penroseState);

  const numStepsPerYield = 1;
  // on message, we will take a step, but these messages will be queued behind
  // self.onmessage, so we can interrupt optimization with a message
  const optStepMsgChannel = new MessageChannel();

  // take one optimization step
  const optStep = () => {
    try {
      if (optimizerShouldStop) {
        // set by onmessage if we need to stop
        log.info("Optimization finishing early");
        optimizerShouldStop = false;
        return;
      }

      let j = 0;
      const steppedState = step(penroseState!, {
        until: (): boolean => j++ >= numStepsPerYield,
      });
      if (steppedState.isErr()) {
        stepSequenceInfo.state = {
          tag: "OptimizationError",
          error: steppedState.error,
        };
        return;
      } else {
        const stepped = steppedState.value;
        if (isOptimized(stepped) && !finalStage(stepped)) {
          const nextInitState = nextStage(stepped);
          penroseState = nextInitState;
          const currentStage =
            penroseState.optStages[penroseState.currentStageIndex];
          // add the total steps taken by the previous stage
          stepSequenceInfo.layoutStats.push({
            name: currentStage,
            steps: 0,
            cumulativeSteps:
              stepSequenceInfo.layoutStats.at(-1)!.cumulativeSteps,
          });
        } else {
          penroseState = stepped;
        }

        varyingValuesSequence.push([...penroseState.varyingValues]);
        stepSequenceInfo.layoutStats.at(-1)!.steps++;
        stepSequenceInfo.layoutStats.at(-1)!.cumulativeSteps++;
      }

      if (isOptimized(penroseState)) {
        log.info("Optimization finished");
        stepSequenceInfo.state = "Done";
        return;
      }

      optStepMsgChannel.port2.postMessage(null);
    } catch (err: any) {
      log.info("Optimization failed. Quitting without finishing...");
      stepSequenceInfo.state = {
        tag: "OptimizationError",
        error: err,
      };
      return;
    }
  };

  optStepMsgChannel.port1.onmessage = () => {
    optStep();
  };
  optStepMsgChannel.port2.postMessage(null);
};

const resample = (variation: string): ResampleResult => {
  penroseState = penroseResample({
    ...penroseState!,
    variation,
  });
  const stepSequenceId = makeNewStepSequence(null, variation);
  startOptimize(stepSequenceId);

  return taggedOk(MessageTags.Resample, stepSequenceId);
};

self.onmessage = async ({
  data,
}: MessageEvent<MessageRequest | Notification>) => {
  switch (data.tag) {
    case "MessageRequest":
      {
        const requestData = data.data;
        log.info(`Worker recieved request ${requestData.tag}`);

        let result: MessageResult;
        switch (requestData.tag) {
          case MessageTags.Compile:
            result = await compile(requestData);
            break;

          case MessageTags.Poll:
            result = await poll();
            break;

          case MessageTags.ComputeLayout:
            result = await computeLayout(requestData);
            break;

          case MessageTags.Resample:
            result = resample(requestData.variation);
            break;

          // never
          // default:
          //   throw new Error(
          //     `Request type ${requestData.tag} not supported for worker`,
          //   );
        }

        respond(data.messageId, result);
      }
      break;

    case "Notification":
      {
        const notifData = data.data;
        log.info(`Worker recieved notification ${notifData.tag}`);
        switch (notifData.tag) {
          case MessageTags.LabelMeasurements:
            const partialState = {
              ...penroseState!,
              labelCache: notifData.labelMeasurements,
            };
            penroseState = insertPending(partialState) as PenroseState;
            startOptimize(0);
            break;

          default:
            throw new Error(
              `Notification type ${notifData.tag} not supported from main thread to broker`,
            );
        }
      }
      break;
  }

  //
};

log.info("Worker initialized");
notify({ tag: MessageTags.Init });
