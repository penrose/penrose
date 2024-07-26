import {
  Canvas,
  collectLabels,
  compileCompGraph,
  finalStage,
  genGradient,
  getTranslatedInputsIdxs,
  IdxsByPath,
  InputInfo,
  insertPending,
  isOptimized,
  mathjaxInit,
  nextStage,
  Num,
  PenroseState,
  Shape,
  start,
  step,
} from "@penrose/core";
import consola, { LogLevels } from "consola";
import { DragConstraint } from "../types.js";
import { stateToSVG } from "../utils.js";

const log = consola.create({ level: LogLevels.info }).withTag("diagram");

export type DiagramData = {
  canvas: Canvas;
  variation: string;
  inputs: InputInfo[];
  constraints: Num[];
  objectives: Num[];
  shapes: Shape<Num>[];
  nameShapeMap: Map<string, Shape<Num>>;
  namedInputs: Map<string, number>;
  pinnedInputs: Set<number>;
  dragNamesAndConstrs: Map<string, DragConstraint>;
  inputIdxsByPath: IdxsByPath;
};

export class Diagram {
  private state: PenroseState;

  /**
   * Manually pinned indices (during construction or at runtime). These will remain
   * pinned until unpinned manually.
   * @private
   */
  private manuallyPinnedIndices = new Set<number>();

  /**
   * Map from shape ids to translatable inputs who have been temporarily pinned. These will be
   * unpinned after the drag ends, were manually pinned as well.
   * @private
   */
  private tempPinnedForDrag = new Map<string, number[][]>();
  private draggingConstraints: Map<string, DragConstraint>;
  private namedInputs: Map<string, number>;
  private onInteraction = () => {};
  private inputEffects: Map<string, Set<(val: number, name: string) => void>> =
    new Map();

  /**
   * Create a new renderable diagram. This should not be called directly; use
   * `DiagramBuilder.prototype.build` instead.
   * @param data
   */
  static create = async (data: DiagramData): Promise<Diagram> => {
    return new Diagram(
      await Diagram.makeState(data),
      data.pinnedInputs,
      data.dragNamesAndConstrs,
      data.namedInputs,
    );
  };

  private constructor(
    state: PenroseState,
    pinnedInputs: Set<number>,
    draggingConstraints: Map<string, DragConstraint>,
    namedInputs: Map<string, number>,
  ) {
    this.state = state;
    this.manuallyPinnedIndices = pinnedInputs;
    this.draggingConstraints = draggingConstraints;
    this.namedInputs = namedInputs;
  }

  /**
   * Render an SVG of the current diagram state and a map of shape names to SVG elements.
   */
  render = async () => {
    const shapes = this.state.computeShapes(this.state.varyingValues);
    const titleCache = new Map<string, SVGElement>();
    const svg = await stateToSVG(
      {
        canvas: this.state.canvas,
        shapes,
        labelCache: this.state.labelCache,
        variation: this.state.variation,
      },
      {
        pathResolver: async () => {
          throw new Error("File loading not supported");
        },
        width: "100%",
        height: "100%",
        texLabels: false,
        titleCache,
      },
    );

    return {
      svg,
      nameElemMap: titleCache,
    };
  };

  /**
   * Take one optimization step. This will update the internal state of the diagram.
   * @returns `true` if the optimization should continue, `false` if it should stop
   * (i.e. when optimization converges or fails)
   */
  optimizationStep = async () => {
    try {
      let i = 0;
      const steppedState = step(this.state, {
        until: (): boolean => i++ >= 1,
      });
      if (steppedState.isErr()) {
        throw steppedState.error;
      } else {
        // if we successfully took an optimization step
        const stepped = steppedState.value;
        this.triggerInputEffects(
          this.state.varyingValues,
          stepped.varyingValues,
        );
        if (isOptimized(stepped) && !finalStage(stepped)) {
          // if we should go to the next layout stage
          this.state = nextStage(stepped);
        } else {
          // if we should stay in the current layout stage
          this.state = stepped;
        }
      } // end if successfully optimized

      if (isOptimized(this.state)) {
        log.info("Optimization finished");
        return false; // from the opt step
      } else {
        return true;
      }
    } catch (err: any) {
      log.info("Optimization failed. Quitting without finishing...");
      return false;
    }
  };

  beginDrag = (name: string) => {
    this.tempPinnedForDrag.set(name, getTranslatedInputsIdxs(name, this.state));
    this.applyPins(this.state);
  };

  endDrag = (name: string) => {
    this.tempPinnedForDrag.delete(name);
    this.applyPins(this.state);
  };

  translate = (name: string, dx: number, dy: number) => {
    if (!this.tempPinnedForDrag.has(name)) {
      throw new Error(`Inputs were not pinned before translating ${name}`);
    }

    this.onInteraction();
    this.resetOptimization();

    const translatedIndices = this.tempPinnedForDrag.get(name)!;
    const prevVaryingValues = [...this.state.varyingValues];
    for (const [xIdx, yIdx] of translatedIndices) {
      this.state.varyingValues[xIdx] += dx;
      this.state.varyingValues[yIdx] += dy;
    }
    this.triggerInputEffects(prevVaryingValues, this.state.varyingValues);
  };

  getInput = (name: string) => {
    const idx = this.namedInputs.get(name);
    if (idx === undefined) {
      throw new Error(`No input named ${name}`);
    }
    return this.state.varyingValues[idx];
  };

  setInput = (name: string, val: number, triggerEffects = true) => {
    const idx = this.namedInputs.get(name);
    if (idx === undefined) {
      throw new Error(`No input named ${name}`);
    }
    this.state.varyingValues[idx] = val;
    this.resetOptimization();
    this.onInteraction();
    if (triggerEffects && this.inputEffects.has(name)) {
      for (const effect of this.inputEffects.get(name)!) {
        effect(val, name);
      }
    }
  };

  getPinned = (name: string) => {
    const idx = this.namedInputs.get(name);
    if (idx === undefined) {
      throw new Error(`No input named ${name}`);
    }
    return this.manuallyPinnedIndices.has(idx);
  };

  setPinned = (name: string, pinned: boolean) => {
    const idx = this.namedInputs.get(name);
    if (idx === undefined) {
      throw new Error(`No input named ${name}`);
    }
    if (pinned) {
      this.manuallyPinnedIndices.add(idx);
    } else {
      this.manuallyPinnedIndices.delete(idx);
    }
    this.applyPins(this.state);
    this.resetOptimization();
    this.onInteraction();
  };

  getCanvas = () => ({ ...this.state.canvas });

  getDraggingConstraints = () => new Map(this.draggingConstraints);

  setOnInteraction = (fn: () => void) => (this.onInteraction = fn);

  addInputEffect = (name: string, fn: (val: number, name: string) => void) => {
    if (!this.namedInputs.has(name)) {
      throw new Error(`No input named ${name}`);
    }
    if (!this.inputEffects.has(name)) {
      this.inputEffects.set(name, new Set());
    }
    this.inputEffects.get(name)!.add(fn);
  };

  removeInputEffect = (
    name: string,
    fn: (val: number, name: string) => void,
  ) => {
    if (!this.inputEffects.has(name)) {
      throw new Error(`No input named ${name}`);
    }
    this.inputEffects.get(name)!.delete(fn);
  };

  private triggerInputEffects = (
    prevVaryingValues: number[],
    newVaryingValues: number[],
  ) => {
    for (const [name, effects] of this.inputEffects) {
      if (name === "ihat.x") {
        console.log("hi");
      }
      const idx = this.namedInputs.get(name)!;
      if (prevVaryingValues[idx] !== newVaryingValues[idx]) {
        for (const effect of effects) {
          effect(newVaryingValues[idx], name);
        }
      }
    }
  };

  private resetOptimization = () => {
    this.state.params = start(this.state.varyingValues.length);
    this.state.currentStageIndex = 0;
  };

  private applyPins = (state: PenroseState) => {
    const inputMask = state.inputs.map(
      ({ meta }, i) =>
        meta.init.tag === "Sampled" && !this.manuallyPinnedIndices.has(i),
    );
    for (const [_, pinnedIndices] of this.tempPinnedForDrag) {
      for (const [xIdx, yIdx] of pinnedIndices) {
        inputMask[xIdx] = false;
        inputMask[yIdx] = false;
      }
    }
    state.constraintSets = new Map([
      [
        "",
        {
          inputMask,
          objMask: state.constraintSets.values().next()!.value.objMask,
          constrMask: state.constraintSets.values().next()!.value.constrMask,
        },
      ],
    ]);
  };

  private static makeState = async (
    data: DiagramData,
  ): Promise<PenroseState> => {
    const constraintSets = new Map([
      [
        "",
        {
          inputMask: data.inputs.map(
            ({ meta }, i) =>
              meta.init.tag === "Sampled" && !data.pinnedInputs.has(i),
          ),
          objMask: data.objectives.map(() => true),
          constrMask: data.constraints.map(() => true),
        },
      ],
    ]);

    const inputVars = data.inputs.map((i) => i.handle);
    const inputVals = inputVars.map((v) => v.val);

    const state: PenroseState = {
      warnings: [],
      variation: data.variation,
      constraintSets,
      objFns: [],
      constrFns: [],
      varyingValues: inputVals,
      inputs: data.inputs,
      labelCache: new Map(),
      shapes: data.shapes,
      canvas: data.canvas,
      currentStageIndex: 0,
      optStages: [""],
      params: start(data.inputs.length),
      gradient: await genGradient(inputVars, data.objectives, data.constraints),
      computeShapes: await compileCompGraph(inputVars, data.shapes),
      interactivityInfo: {
        inputIdxsByPath: data.inputIdxsByPath,
        translatableShapePaths: new Set(data.dragNamesAndConstrs.keys()),
        scalableShapePaths: new Set(),
        // currently, penrose ide needs dragging constrants to be part of state,
        // but we keep track separately
        draggingConstraints: new Map(),
        shapesByPath: data.nameShapeMap,
      },
    };

    const shapes = state.computeShapes(state.varyingValues);
    const convert = mathjaxInit();
    const labelCache = await collectLabels(shapes, convert);
    if (labelCache.isErr()) {
      throw labelCache.error;
    }

    return insertPending({
      ...state,
      labelCache: labelCache.value,
    });
  };
}
