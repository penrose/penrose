import {
  compileTrio,
  finalStage,
  isOptimized,
  LabelCache,
  LabelMeasurements,
  nextStage,
  PenroseState,
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
  HistoryInfo,
  HistoryLoc,
  logLevel,
  MessageRequest,
  MessageResult,
  MessageTags,
  notify,
  PollResult,
  removeRenderedLabels,
  respond,
  StepSequenceID,
  StepSequenceIDGenerator,
  StepSequenceInfo,
  taggedErr,
  taggedOk,
} from "./common.js";

const log = consola.create({ level: logLevel }).withScope("optimizer:worker");

const historyInfo: HistoryInfo = new Map();
/**
 * Varying values sequence for each step sequence. Kept separate from history
 * info so we don't need to recompute history info on every poll.
 */
const historyValues: Map<StepSequenceID, number[][]> = new Map();

const stepSequenceIdGenerator = new StepSequenceIDGenerator();

/**
 * Context for optimizer. Varying values only represent the last
 * values the optimizer saw; refer to `historyValue` to get the appropriate list
 * for a particular `historyLoc`.
 */
let penroseState: PenroseState | null = null;
let optimizerShouldStop = false;

const getLabelMeasurements = (labelCache: LabelCache): LabelMeasurements => {
  const result: LabelMeasurements = new Map();
  for (const [name, label] of labelCache.entries()) {
    result.set(name, removeRenderedLabels(label));
  }
  return result;
};
const compile = async (data: CompileRequestData): Promise<CompileResult> => {
  const compiledState = await compileTrio(data);

  let result: CompileResult;
  if (Result.isOk(compiledState)) {
    log.info("Compiled");
    penroseState = compiledState.value;

    const stepSequenceId = makeNewStepSequence(null, data.variation);
    startOptimize(stepSequenceId, penroseState.varyingValues);

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
  return taggedOk(MessageTags.Poll, historyInfo);
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
    result = taggedOk(MessageTags.ComputeLayout, {
      canvas: penroseState!.canvas,
      shapes: penroseState!.computeShapes(xs),
      labelMeasurements: getLabelMeasurements(penroseState!.labelCache),
      variation: stepSequenceInfo.variation,
    });
  } else {
    result = taggedErr(MessageTags.ComputeLayout, {
      tag: "InvalidHistoryLocError",
      historyLoc: data.historyLoc,
      historyInfo,
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
        steps: 0,
        cumulativeSteps: 0,
      },
    ],
    parent: parentLoc,
    state: "Pending",
    variation,
  };
  const varyingValuesSequence: number[][] = [];

  historyInfo.set(id, info);
  historyValues.set(id, varyingValuesSequence);
  return id;
};

const startOptimize = async (
  stepSequenceId: StepSequenceID,
  initVaryingValues: number[],
) => {
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

  penroseState = {
    ...penroseState!,
    varyingValues: [...initVaryingValues],
    params: start(initVaryingValues.length),
    currentStageIndex: stepSequenceInfo.layoutStats.length - 1,
  };

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
      varyingValuesSequence.push([...penroseState!.varyingValues]);
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
          // update the step count for the current stage
          stepSequenceInfo.layoutStats.at(-1)!.steps++;
        }

        varyingValuesSequence.push([...penroseState.varyingValues]);
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

self.onmessage = async ({ data }: MessageEvent<MessageRequest>) => {
  const requestData = data.data;
  log.info(`Worker recieved ${requestData.tag}`);

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

    default:
      throw new Error(
        `Request type ${requestData.tag} not supported for worker`,
      );
  }

  respond(data.messageId, result);
};

log.info("Worker initialized");
notify({ tag: "InitData" });
