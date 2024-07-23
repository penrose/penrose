import {
  Canvas,
  collectLabels,
  compileCompGraph,
  finalStage,
  genGradient,
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

const log = consola.create({ level: LogLevels.warn }).withTag("diagram");

export type DiagramData = {
  canvas: Canvas;
  variation: string;
  inputs: InputInfo[];
  constraints: Num[];
  objectives: Num[];
  shapes: Shape<Num>[];
  idShapeMap: Map<string, Shape<Num>>;
  namedInputs: Map<string, number>;
  pinnedInputs: Set<number>;
  dragIdsAndConstrs: Map<string, DragConstraint>;
};

export class Diagram {
  private state: PenroseState;

  static create = async (data: DiagramData): Promise<Diagram> => {
    return new Diagram(await Diagram.makeState(data));
  };

  private constructor(state: PenroseState) {
    this.state = state;
  }

  render = async () => {
    const shapes = this.state.computeShapes(this.state.varyingValues);
    return await stateToSVG(
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
      },
    );
  };

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
        inputIdxsByPath: new Map(),
        translatableShapePaths: new Set(data.dragIdsAndConstrs.keys()),
        scalableShapePaths: new Set(),
        // currently, penrose ide needs dragging constrants to be part of state,
        // but we keep track separately
        draggingConstraints: new Map(),
        shapesByPath: data.idShapeMap,
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
