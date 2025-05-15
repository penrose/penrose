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
  makeTranslateOnMouseDown,
  mul,
  nextStage,
  Num,
  ops,
  PenroseState,
  Shape,
  start,
  step,
} from "@penrose/core";
import consola, { LogLevels } from "consola";
import { mathjax } from "mathjax-full/js/mathjax.js";
import { SharedInput } from "./builder.js";
import { DragConstraint } from "./types.js";
import {
  CallbackLooper,
  mathjaxInitWithHandler,
  setNoFillIfTransparent,
  stateToSVG,
} from "./utils.js";

const log = consola.create({ level: LogLevels.warn }).withTag("diagram");

/** Data passed into `create` */
export type DiagramCreationData = {
  canvas: Canvas;
  variation: string;
  inputs: InputInfo[];
  constraints: Num[];
  objectives: Num[];
  shapes: Shape<Num>[];
  nameShapeMap: Map<string, Shape<Num>>;
  namedInputs: Map<string, number>;
  pinnedInputs: Set<number>;
  draggingConstraints: Map<string, DragConstraint>;
  inputIdxsByPath: IdxsByPath;
  lassoStrength: number;
  sharedInputs: Set<SharedInput>;
  interactiveOnlyShapes: Set<Shape<Num>>;
  eventListeners: Map<string, [string, (e: any, diagram: Diagram) => void][]>;
};

/** Data passed into the constructor. Has had state create asynchronously. */
type DiagramConstructorData = {
  state: PenroseState;
  pinnedInputs: Set<number>;
  draggingConstraints: Map<string, DragConstraint>;
  namedInputs: Map<string, number>;
  sharedInputs: Set<SharedInput>;
  lassoStrength: number;
  interactiveOnlyShapes: Set<Shape<Num>>;
  eventListeners: Map<string, [string, (e: any, diagram: Diagram) => void][]>;
};

/**
 * A renderable diagram, created with `DiagramBuilder.prototype.build`.
 * You should pass a factory method to `useDiagram` to create a `Diagram` from
 * within a React component, for performance reasons.
 */
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
  private lassoEnabled: boolean;
  private sharedInputs = new Set<SharedInput>();
  private interactiveOnlyShapes;
  private eventListeners;
  private optimizationLooper = new CallbackLooper("MessageChannel");
  private renderLooper = new CallbackLooper("AnimationFrame");
  private onOptimizationFinished = (xs: number[]) => {};
  private onOptimizationStepped = (xs: number[]) => {};
  private onOptimizationStarted = (xs: number[]) => {};

  /**
   * Create a new renderable diagram. This should not be called directly; use
   * `DiagramBuilder.prototype.build` instead.
   * @param data
   */
  static create = async (data: DiagramCreationData): Promise<Diagram> => {
    return new Diagram({
      state: await Diagram.makeState(data),
      ...data,
    });
  };

  private constructor(data: DiagramConstructorData) {
    this.state = data.state;
    this.manuallyPinnedIndices = data.pinnedInputs;
    this.draggingConstraints = data.draggingConstraints;
    this.namedInputs = data.namedInputs;
    this.lassoEnabled = data.lassoStrength !== 0;
    this.sharedInputs = data.sharedInputs;
    this.interactiveOnlyShapes = data.interactiveOnlyShapes;
    this.eventListeners = data.eventListeners;
  }

  /**
   * Render an SVG of the current diagram state and a map of shape names to SVG elements.
   */
  render = async () => {
    const titleCache = new Map<string, SVGElement>();
    const svg = await stateToSVG(this.state, {
      // penrose expects path resolving to string, we pass the resolving on to the
      // user. So the svg "href" at this point is already a raw svg string.
      pathResolver: async (str) => str,
      texLabels: false,
      titleCache,
    });

    return {
      svg,
      nameElemMap: titleCache,
    };
  };

  /**
   *
   */
  renderStatic = async () => {
    const { svg, nameElemMap } = await this.render();
    for (const shape of this.state.shapes) {
      if (this.interactiveOnlyShapes.has(shape)) {
        const elem = nameElemMap.get(shape.name.contents)!;
        elem.remove();
      }
    }
    return svg;
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
        this.onOptimizationFinished(this.copyNonLassoVaryingVals());
        return false; // from the opt step
      } else {
        this.onOptimizationStepped(this.copyNonLassoVaryingVals());
        return true;
      }
    } catch (err: unknown) {
      log.info(`Optimization failed: ${err}`);
      this.onOptimizationFinished(this.copyNonLassoVaryingVals());
      return false;
    }
  };

  private copyNonLassoVaryingVals = () => {
    if (this.lassoEnabled) {
      return this.state.varyingValues.slice(
        0,
        this.state.varyingValues.length / 2,
      );
    } else {
      return this.state.varyingValues.slice();
    }
  };

  private initialRender = async (): Promise<{
    svg: SVGElement;
    nameElemMap: Map<string, SVGElement>;
    draggingRef: { dragging: boolean };
  }> => {
    while (await this.optimizationStep()) {
      /* empty */
    }

    const { svg, nameElemMap } = await this.render();
    const draggingRef = { dragging: false };
    for (const [name, elem] of nameElemMap) {
      setNoFillIfTransparent(elem);
      elem.setAttribute("pointer-events", "painted");
      if (this.draggingConstraints.has(name)) {
        // get rid of tooltip
        elem.insertBefore(
          document.createElementNS("http://www.w3.org/2000/svg", "title"),
          elem.firstChild,
        );
        let lastDx = 0;
        let lastDy = 0;
        const translateFn = makeTranslateOnMouseDown(
          svg,
          elem,
          this.getCanvas(),
          name,
          async (path, dx, dy) => {
            this.translate(path, dx - lastDx, dy - lastDy);
            lastDx = dx;
            lastDy = dy;
          },
          ([x, y]) => this.draggingConstraints.get(name)!([x, y], this),
          undefined,
          () => {
            this.endDrag(name);
            draggingRef.dragging = false;
            lastDx = 0;
            lastDy = 0;
          },
        );
        elem.addEventListener("pointerdown", (e) => {
          this.beginDrag(name);
          draggingRef.dragging = true;
          translateFn(e);
        });
      }
      if (this.eventListeners.has(name)) {
        const listeners = this.eventListeners.get(name)!;
        for (const [event, listener] of listeners) {
          elem.addEventListener(event, (e: any) => listener(e, this));
        }
      }
    }
    svg.setAttribute("preserveAspectRatio", "xMidYMid meet");
    svg.setAttribute("pointer-events", "none");
    svg.style.width = "100%";
    svg.style.height = "100%";

    return { svg, nameElemMap, draggingRef };
  };

  private copyAttrs = (src: Element, dest: Element) => {
    for (let i = 0; i < src.children.length; i++) {
      this.copyAttrs(src.children[i], dest.children[i]);
    }

    for (let i = 0; i < src.attributes.length; i++) {
      const attr = src.attributes[i];
      dest.setAttribute(attr.name, attr.value);
    }
  };

  /**
   * Returns an HTMLElement presenting an interactive diagram. This element should
   * be appended added to an existing document node to be visible. The element is
   * styled by default with "width: 100%; height: 100%l touch-action: none".
   */
  getInteractiveElement = () => {
    const parentElement = document.createElement("div");
    parentElement.style.height = "100%";
    parentElement.style.width = "100%";
    parentElement.style.touchAction = "none";

    let svg: SVGElement | null = null;
    let nameElemMap: Map<string, SVGElement> | null = null;
    let draggingRef: { dragging: boolean } | null = null;

    const optimizationLoop = async () => {
      return await this.optimizationStep();
    };

    const renderLoop = async () => {
      if (svg === null || nameElemMap === null || draggingRef === null) {
        const {
          svg: newSvg,
          nameElemMap: newNameElemMap,
          draggingRef: newDraggingRef,
        } = await this.initialRender();

        svg = newSvg;
        nameElemMap = newNameElemMap;
        draggingRef = newDraggingRef;

        parentElement.appendChild(svg);
      } else {
        const { nameElemMap: newNameElemMap } = await this.render();

        for (const [name, elem] of newNameElemMap) {
          const oldElem = nameElemMap.get(name);
          if (oldElem) {
            this.copyAttrs(elem, oldElem);
            if (this.draggingConstraints.has(name)) {
              oldElem.setAttribute(
                "cursor",
                draggingRef.dragging ? "grabbing" : "grab",
              );
            }
            setNoFillIfTransparent(oldElem);
          } else {
            throw new Error(`Shape ${name} not found in old element map`);
          }
        }
      }

      return this.optimizationLooper.isRunning();
    };

    this.setOnInteraction(() => {
      this.optimizationLooper.loop(optimizationLoop);
      this.renderLooper.loop(renderLoop);
    });

    this.optimizationLooper.loop(optimizationLoop);
    this.renderLooper.loop(renderLoop);

    return parentElement;
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
    if (this.lassoEnabled) this.setAndEnableLasso();

    const translatedIndices = this.tempPinnedForDrag.get(name)!;
    const prevVaryingValues = [...this.state.varyingValues];
    for (const [xIdx, yIdx] of translatedIndices) {
      this.state.varyingValues[xIdx] += dx;
      this.state.varyingValues[yIdx] += dy;
    }
    this.triggerInputEffects(prevVaryingValues, this.state.varyingValues);
  };

  /**
   * Get the value of an input by name.
   * @param name The name of the input
   */
  getInput = (name: string) => {
    const idx = this.namedInputs.get(name);
    if (idx === undefined) {
      throw new Error(`No input named ${name}`);
    }
    return this.state.varyingValues[idx];
  };

  /**
   * Set the value of an input by name.
   * @param name The name of the input
   * @param val The new value
   */
  setInput = (name: string, val: number) => {
    const idx = this.namedInputs.get(name);
    if (idx === undefined) {
      throw new Error(`No input named ${name}`);
    }
    this.state.varyingValues[idx] = val;
    this.resetOptimization();
    this.onInteraction();
    if (this.lassoEnabled) this.setAndEnableLasso();
    if (this.inputEffects.has(name)) {
      for (const effect of this.inputEffects.get(name)!) {
        effect(val, name);
      }
    }
  };

  /**
   * Get the value of an input by index.
   * @param idx The index of the input
   */
  getOptimized = (name: string) => {
    const idx = this.namedInputs.get(name);
    if (idx === undefined) {
      throw new Error(`No input named ${name}`);
    }
    return !this.manuallyPinnedIndices.has(idx);
  };

  /**
   * Set the value of an input by index.
   * @param idx The index of the input
   * @param optimized Whether the input should be optimized
   */
  setOptimized = (name: string, optimized: boolean) => {
    const idx = this.namedInputs.get(name);
    if (idx === undefined) {
      throw new Error(`No input named ${name}`);
    }
    if (!optimized) {
      this.manuallyPinnedIndices.add(idx);
    } else {
      this.manuallyPinnedIndices.delete(idx);
    }
    this.applyPins(this.state);
    this.resetOptimization();
    this.onInteraction();
    if (this.lassoEnabled) this.setAndEnableLasso();
  };

  /**
   * Get the canvas of the diagram.
   */
  getCanvas = () => ({ ...this.state.canvas });

  /**
   * Get the dragging constraints of the diagram.
   */
  getDraggingConstraints = () => new Map(this.draggingConstraints);

  setOnInteraction = (fn: () => void) => (this.onInteraction = fn);

  /**
   * Add an effect to an input. The effect will be called any time the input changes.
   * @param name The name of the input
   * @param fn The effect function
   */
  addInputEffect = (name: string, fn: (val: number, name: string) => void) => {
    if (!this.namedInputs.has(name)) {
      throw new Error(`No input named ${name}`);
    }
    if (!this.inputEffects.has(name)) {
      this.inputEffects.set(name, new Set());
    }
    this.inputEffects.get(name)!.add(fn);
  };

  setOnOptimizationFinished = (fn: (xs: number[]) => void) => {
    this.onOptimizationFinished = fn;
  };

  setOnOptimizationStepped = (fn: (xs: number[]) => void) => {
    this.onOptimizationStepped = fn;
  };

  setOnOptimizationStarted = (fn: (xs: number[]) => void) => {
    this.onOptimizationStarted = fn;
  };

  /**
   * Remove an effect from an input.
   * @param name The name of the input
   * @param fn The effect function
   */
  removeInputEffect = (
    name: string,
    fn: (val: number, name: string) => void,
  ) => {
    if (!this.inputEffects.has(name)) {
      throw new Error(`No input named ${name}`);
    }
    this.inputEffects.get(name)!.delete(fn);
  };

  discard = () => {
    for (const input of this.sharedInputs) {
      input.unregister(this);
    }
  };

  private setAndEnableLasso = () => {
    for (let i = 0; i < this.state.varyingValues.length / 2; ++i) {
      this.state.varyingValues[i + this.state.varyingValues.length / 2] =
        this.state.varyingValues[i];
    }
    const objMask = this.state.constraintSets.get("")!.objMask;
    objMask[objMask.length - 1] = true;
  };

  private triggerInputEffects = (
    prevVaryingValues: number[],
    newVaryingValues: number[],
  ) => {
    for (const [name, effects] of this.inputEffects) {
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
    this.onOptimizationStarted(this.copyNonLassoVaryingVals());
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
    data: DiagramCreationData,
  ): Promise<PenroseState> => {
    // copy since we might append to
    const constraints = data.constraints.slice();
    const objectives = data.objectives.slice();

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

    if (data.lassoStrength !== 0) {
      // add lasso term, disabled by default
      objectives.push(
        mul(
          data.lassoStrength,
          ops.vdist(
            data.inputs.slice(0, data.inputs.length / 2).map((i) => i.handle),
            data.inputs.slice(data.inputs.length / 2).map((i) => i.handle),
          ),
        ),
      );
      constraintSets.get("")!.objMask.push(false);
    }

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
      gradient: await genGradient(inputVars, objectives, constraints),
      computeShapes: await compileCompGraph(inputVars, data.shapes),
      interactivityInfo: {
        inputIdxsByPath: data.inputIdxsByPath,
        translatableShapePaths: new Set(data.draggingConstraints.keys()),
        scalableShapePaths: new Set(),
        // currently, penrose ide needs dragging constrants to be part of state,
        // but we keep track separately
        draggingConstraints: new Map(),
        shapesByPath: data.nameShapeMap,
      },
    };

    const shapes = state.computeShapes(state.varyingValues);
    const { convert, handler } = mathjaxInitWithHandler();
    const labelCache = await collectLabels(shapes, convert);
    mathjax.handlers.unregister(handler);

    if (labelCache.isErr()) {
      throw labelCache.error;
    }

    return insertPending({
      ...state,
      labelCache: labelCache.value,
    });
  };
}
