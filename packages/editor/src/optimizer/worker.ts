import {
  PenroseState,
  StagedConstraints,
  compileTrio,
  finalStage,
  getAllInteractiveIdxs,
  insertPending,
  isOptimized,
  nextStage,
  resample as penroseResample,
  scaleVaryingValues,
  start,
  step,
  translateVaryingValues,
} from "@penrose/core";
import consola from "consola";
import { cloneDeep } from "lodash";
import { Result } from "true-myth";
import {
  CompileRequestData,
  CompileResult,
  ComputeLayoutRequestData,
  ComputeLayoutResult,
  HistoryLoc,
  InteractionRequestData,
  InteractionResult,
  MessageRequest,
  MessageResult,
  MessageTags,
  Notification,
  PollResult,
  ResampleResult,
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

const log = consola.create({ level: logLevel }).withTag("optimizer:worker");
const stepSequenceIdGenerator = new StepSequenceIDGenerator();
// on message, we will take a step, but these messages will be queued behind
// self.onmessage, so we can interrupt optimization with a message
const optStepMsgChannel = new MessageChannel();

let historyInfo = new Map<StepSequenceID, StepSequenceInfo>();
/**
 * Varying values sequence for each step sequence. Kept separate from history
 * info so we don't need to recompute history info on every poll.
 */
let historyValues: Map<StepSequenceID, number[][]> = new Map();

/**
 * Context for optimizer. Varying values only represent the last
 * values the optimizer saw; refer to `historyValue` to get the appropriate list
 * for a particular `historyLoc`. This will be non-null always after compilation,
 * which happens immediately after launch, so it is virtually always safe to use
 * `penroseState!`.
 */
let penroseState: PenroseState | null = null;
let activeOptimizationId: number = 0;
let unpinnedContraintSets: StagedConstraints = new Map();
let interacting = false;

/**
 * Compile, but don't start optimizing until we receive `LabelMeasurementsData`
 * in a notification. After calling, `penroseState` will be non-null.
 * @param data
 */
const compile = async (data: CompileRequestData): Promise<CompileResult> => {
  const compiledState = await compileTrio(data);

  let result: CompileResult;
  if (Result.isOk(compiledState)) {
    log.info("Compiled");
    penroseState = compiledState.value;

    // make copy to apply pins to
    unpinnedContraintSets = cloneDeep(penroseState.constraintSets);

    makeNewStepSequence(null, data.variation, [...penroseState.varyingValues]);
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
  const varyingValuesSequence = historyValues.get(data.historyLoc.sequenceId);
  const stepSequenceInfo = historyInfo.get(data.historyLoc.sequenceId);

  let result: ComputeLayoutResult;
  if (
    varyingValuesSequence &&
    stepSequenceInfo &&
    0 <= data.historyLoc.frame &&
    data.historyLoc.frame < varyingValuesSequence.length
  ) {
    const xs = varyingValuesSequence[data.historyLoc.frame];
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
      historyInfo,
    });
  }

  return result;
};

const recursivelyDeleteStepSequence = (sequenceId: StepSequenceID) => {
  let currSeqId: StepSequenceID | null = sequenceId;
  while (currSeqId !== null) {
    const info = historyInfo.get(currSeqId);

    historyInfo.delete(currSeqId);
    historyInfo.delete(currSeqId);

    currSeqId = info?.child ?? null;
  }
};

/**
 * Create a new step sequence, with 1 frame containing `initVaryingValues`.
 * If the sequence pointed to by `parentLoc.sequenceId` already has a child,
 * that child and its ancestors are recursively deleted.
 * @param parentLoc
 * @param variation
 * @param initVaryingValues
 */
const makeNewStepSequence = (
  parentLoc: HistoryLoc | null,
  variation: string,
  initVaryingValues?: number[],
): StepSequenceID => {
  const id = stepSequenceIdGenerator.next();

  // set new parent's child to us
  let parentSeq;
  if (parentLoc !== null) {
    parentSeq = historyInfo.get(parentLoc.sequenceId);
    if (parentSeq !== undefined) {
      if (parentSeq.child) {
        recursivelyDeleteStepSequence(parentSeq.child);
      }
      parentSeq.child = id;
    }
  }

  const info: StepSequenceInfo = {
    layoutStats: [
      {
        name: interacting
          ? "interaction"
          : penroseState!.optStages.length === 1
          ? "default"
          : penroseState!.optStages[0],
        frames: initVaryingValues ? 1 : 0,
        cumulativeFrames: initVaryingValues ? 1 : 0,
      },
    ],
    parent: parentLoc,
    child: null,
    state: { tag: "Pending" },
    variation,
    pinnedInputPaths: new Map(parentSeq?.pinnedInputPaths),
  };

  historyInfo.set(id, info);
  historyValues.set(id, initVaryingValues ? [initVaryingValues] : []);
  return id;
};

/**
 * Begin optimizing a step sequence from where it left off. This function resets
 * `penroseState`s optimizer params, sets the step sequence state to pending,
 * and then starts an optimization loop, yielding every step for new messages.
 *
 * The loop exists when the optimizer is done or `startOptimize` is called again.
 * @param stepSequenceId
 * @param initVaryingValues Values to push to history and start from
 */
const startOptimize = (
  stepSequenceId: StepSequenceID,
  initVaryingValues?: number[],
) => {
  // we can't just use stepSequenceId because we might want to interrupt
  // and continue on same sequence (e.g., for interaction)
  activeOptimizationId++;
  const startingOptimizationId = activeOptimizationId;

  const stepSequenceInfo = historyInfo.get(stepSequenceId);
  if (!stepSequenceInfo) {
    // we don't even have a step sequence to set to error state, so we have no
    // choice but to throw the error
    throw new Error("invalid step sequence for optimizer");
  }

  stepSequenceInfo.state = { tag: "Pending" };

  const varyingValuesSequence = historyValues.get(stepSequenceId);
  if (
    !varyingValuesSequence ||
    (!initVaryingValues && varyingValuesSequence.length === 0)
  ) {
    stepSequenceInfo.state = {
      tag: "OptimizationError",
      error: new Error(
        `No varying values found for step sequence id ${stepSequenceId}`,
      ),
    };
    return;
  }

  if (initVaryingValues) {
    initVaryingValues = [...initVaryingValues]; // so we don't mutate parameter
    varyingValuesSequence.push(initVaryingValues);
    stepSequenceInfo.layoutStats.at(-1)!.frames++;
    stepSequenceInfo.layoutStats.at(-1)!.cumulativeFrames++;
  } else {
    initVaryingValues = varyingValuesSequence.at(-1)!;
  }

  penroseState = {
    ...penroseState!,
    varyingValues: initVaryingValues,
    params: start(initVaryingValues.length),
    currentStageIndex: 0,
  };

  penroseState = insertPending(penroseState);

  const numStepsPerYield = 1;

  // take one optimization step
  const optStep = () => {
    try {
      // changed if `startOptimize` is called again.
      // e.g. recompile, resample, drag, ...
      if (activeOptimizationId !== startingOptimizationId) {
        // we've been interrupted
        log.info("Optimization finishing early");
        // return from the opt step
        interacting = false;
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
        // return from the opt step
        interacting = false;
        return;
      } else {
        // if we successfully took an optimization step

        const stepped = steppedState.value;

        // calculate total constraint energy
        const xs = new Float64Array(stepped.varyingValues.length);
        const grad = new Float64Array(stepped.varyingValues.length);
        xs.set(stepped.varyingValues);
        const optOutputs = stepped.gradient(
          stepped.constraintSets.get(stepped.optStages[stepped.currentStageIndex])!,
          xs,
          1,
          grad
        );
        const constraintEnergy = optOutputs.constraints.reduce(
          (acc, c) => acc + Math.max(0, c) ** 2,
          0
        );
        console.log(`TOTAL CONSTRAINT ENERGY: ${constraintEnergy}`);

        if (isOptimized(stepped) && !finalStage(stepped)) {
          // if we should go to the next layout stage

          const nextInitState = nextStage(stepped);
          penroseState = nextInitState;
          const currentStage =
            penroseState.optStages[penroseState.currentStageIndex];
          // if we're interaction, just continue the interaction stage
          if (!interacting) {
            stepSequenceInfo.layoutStats.push({
              name: currentStage,
              frames: 0,
              cumulativeFrames:
                stepSequenceInfo.layoutStats.at(-1)!.cumulativeFrames,
            });
          }
        } else {
          // if we should stay in the current layout stage
          penroseState = stepped;
        }

        // in either switching stages or continuing with the same stage,
        // we would like to record the values and increment the steps for the
        // (potentially new) latest stage
        varyingValuesSequence.push([...penroseState.varyingValues]);
        stepSequenceInfo.layoutStats.at(-1)!.frames++;
        stepSequenceInfo.layoutStats.at(-1)!.cumulativeFrames++;
      } // end if successfully optimized

      if (isOptimized(penroseState)) {
        log.info("Optimization finished");
        stepSequenceInfo.state = { tag: "Done" };
        interacting = false;
        return; // from the opt step
      }

      // trigger a new call to `optStep` at the back of the event queue
      optStepMsgChannel.port2.postMessage(null);
    } catch (err: any) {
      log.info("Optimization failed. Quitting without finishing...");
      interacting = false;
      stepSequenceInfo.state = {
        tag: "OptimizationError",
        error: err,
      };
      return;
    }
  };

  // create a new message channel such that, on any message, we take an `optStep`.
  optStepMsgChannel.port1.onmessage = () => {
    optStep();
  };

  // trigger the first `optStep`
  optStepMsgChannel.port2.postMessage(null);
};

/**
 * Resample the current state, creating a new step sequence, and immediately
 * start optimizing.
 * @param variation
 */
const resample = (variation: string): ResampleResult => {
  penroseState = penroseResample({
    ...penroseState!,
    variation,
    constraintSets: cloneDeep(unpinnedContraintSets),
  });
  const stepSequenceId = makeNewStepSequence(null, variation, [
    ...penroseState.varyingValues,
  ]);
  startOptimize(stepSequenceId);

  return taggedOk(MessageTags.Resample, stepSequenceId);
};

const interact = (data: InteractionRequestData): InteractionResult => {
  interacting = true;

  const parentSeqInfo = historyInfo.get(data.parentHistoryLoc.sequenceId);
  if (
    parentSeqInfo === undefined ||
    parentSeqInfo.layoutStats.length === 0 ||
    parentSeqInfo.layoutStats.at(-1)!.cumulativeFrames <=
      data.parentHistoryLoc.frame
  ) {
    return taggedErr(MessageTags.Interaction, {
      tag: "InvalidHistoryLocError",
      historyLoc: data.parentHistoryLoc,
      historyInfo,
    });
  }

  const parentValues = historyValues.get(data.parentHistoryLoc.sequenceId)![
    data.parentHistoryLoc.frame
  ];

  let currentValues;
  let stepSeqId: StepSequenceID;
  if (parentSeqInfo.child === null) {
    // this is a new interaction
    stepSeqId = makeNewStepSequence(
      data.parentHistoryLoc,
      parentSeqInfo.variation,
    );
    currentValues = [...parentValues];
  } else {
    // we're continuing an interaction
    stepSeqId = parentSeqInfo.child;
    currentValues = [...historyValues.get(stepSeqId)!.at(-1)!];
  }

  const stepSeqInfo = historyInfo.get(stepSeqId)!;

  let newXs: number[];
  switch (data.interaction.tag) {
    case "Translation":
      if (
        !penroseState!.interactivityInfo.translatableShapePaths.has(
          data.interaction.path,
        )
      ) {
        return taggedErr(MessageTags.Interaction, {
          tag: "InteractionError",
          message: "Untranslatable shape",
        });
      }

      newXs = translateVaryingValues(
        data.interaction.path,
        data.interaction.dx,
        data.interaction.dy,
        parentValues,
        currentValues,
        penroseState!,
      );
      addPins(data.interaction.path, stepSeqInfo, penroseState!);
      applyPins(stepSeqInfo, penroseState!);
      break;

    case "Scale":
      if (
        !penroseState!.interactivityInfo.scalableShapePaths.has(
          data.interaction.path,
        )
      ) {
        return taggedErr(MessageTags.Interaction, {
          tag: "InteractionError",
          message: "Unscalable shape",
        });
      }

      newXs = scaleVaryingValues(
        data.interaction.path,
        data.interaction.sx,
        data.interaction.sy,
        parentValues,
        currentValues,
        penroseState!,
      );

      addPins(data.interaction.path, stepSeqInfo, penroseState!);
      applyPins(stepSeqInfo, penroseState!);
      break;

    case "ChangePin":
      if (data.interaction.active) {
        addPins(data.interaction.path, stepSeqInfo, penroseState!);
      } else {
        removePins(data.interaction.path, stepSeqInfo);
      }
      applyPins(stepSeqInfo, penroseState!);
      newXs = currentValues;
  }

  penroseState = {
    ...penroseState!,
    currentStageIndex: 0,
    params: start(penroseState!.varyingValues.length),
  };

  // if we're continuing, we need to push the translated values to history
  // if this is new, makeNewStepSequence already did this for
  startOptimize(stepSeqId, newXs);

  return taggedOk(MessageTags.Interaction, {
    sequenceId: stepSeqId,
    historyInfo,
  });
};

const addPins = (
  path: string,
  stepSequenceInfo: StepSequenceInfo,
  state: PenroseState,
) => {
  stepSequenceInfo.pinnedInputPaths.set(
    path,
    getAllInteractiveIdxs(path, state),
  );
};

const removePins = (path: string, stepSequenceInfo: StepSequenceInfo) => {
  stepSequenceInfo.pinnedInputPaths.delete(path);
};

const applyPins = (stepSequenceInfo: StepSequenceInfo, state: PenroseState) => {
  state.constraintSets = cloneDeep(unpinnedContraintSets);
  for (const [_, masks] of state.constraintSets) {
    for (const [path, set] of stepSequenceInfo.pinnedInputPaths) {
      for (const idx of set) {
        masks.inputMask[idx] = false;
      }
    }
  }
};

self.onmessage = async ({
  data,
}: MessageEvent<MessageRequest | Notification>) => {
  switch (data.tag) {
    case "MessageRequest":
      {
        const requestData = data.data;
        log.info(`Worker received request ${requestData.tag}`);

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

          case MessageTags.Interaction:
            result = interact(requestData);
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
            // this is final piece of info we need to start optimizing after
            // compile
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
};

log.info("Worker initialized");
notify({ tag: MessageTags.Init });
