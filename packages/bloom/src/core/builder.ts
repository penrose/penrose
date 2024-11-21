import {
  Canvas,
  IdxsByPath,
  InputInfo,
  InputMeta,
  Num,
  Shape as PenroseShape,
  Var,
  isVar,
  mul,
  sampleShape,
  simpleContext,
  uniform,
} from "@penrose/core";
import * as constraints from "./constraints.js";
import { Diagram } from "./diagram.js";
import {
  Circle,
  CircleProps,
  Drag,
  DragConstraint,
  Ellipse,
  EllipseProps,
  Equation,
  EquationProps,
  Group,
  GroupProps,
  Image,
  ImageProps,
  Line,
  LineProps,
  Path,
  PathProps,
  Polygon,
  PolygonProps,
  Polyline,
  PolylineProps,
  Predicate,
  Rectangle,
  RectangleProps,
  Shape,
  ShapeProps,
  ShapeType,
  Substance,
  TextProps,
  Type,
  Vec2,
} from "./types.js";
import { fromPenroseShape, sortShapes, toPenroseShape } from "./utils.js";

type NamedSamplingContext = {
  makeInput: (meta: InputMeta, name?: string) => Var;
};

/**
 * A description of which substances should be selected over in a `forall` selector.
 * The keys are the names of the variables to assign, and the values are the types of the substances to assign to them.
 *
 * For example, selecting over all pairs of `Bird`s and single `Tree`s, assigning the birds to `b1` and `b2` and the tree to `t`,
 * could look like:
 * ```ts
 * { b1: Bird, b2: Bird, t: Tree }
 * ```
 */
export type SelectorVars = Record<string, Type>;

/**
 * The assigment to a selection, used as the input to closures provided to `forall`.
 * The keys are the same keys as declared in the provided `SelectorVars`,
 * and are a particular satsifying assignment of substances.
 */
export type SelectorAssignment = Record<string, Substance>;

/**
 * Options for a new input.
 *   - `name`: The name of the input. If provided, the value can be retrieved
 *   from each diagram using the input with `DiagramBuilder.prototype.getInput`.
 *   - `init`: What to initialize the input to. Defaults to random sampling.
 *   - `optimized`: Whether the input should be optimized.
 */
export type InputOpts = {
  name?: string;
  init?: number;
  optimized?: boolean;
};

/**
 * An input that can be shared between diagrams and get/set from outside the diagram.
 * You should call `useSharedInput` to create a shared input, rather than
 * calling `new SharedInput` directly.
 */
export class SharedInput {
  public readonly name: string;
  public readonly init?: number;

  private diagrams = new Set<Diagram>();
  private effectMap = new Map<Diagram, (val: number) => void>();
  private valEffects = new Set<(val: number) => void>();
  private currVal: number | null = null;
  private syncing: boolean = false;
  private optimized: boolean;
  private static nextId = 0;

  constructor(init?: number, optimized = false, name?: string) {
    this.name = name ?? `_input_${SharedInput.nextId++}`;
    this.init = init;
    this.optimized = optimized;
  }

  /**
   * Set the value of the input. This will update the value in all diagrams,
   * and trigger a re-render for the component that created the input.
   * @param val Value to set the input to
   */
  set = (val: number) => {
    this.setValNoSyncing(val);
    this.preventSyncing(() => {
      for (const diagram of this.diagrams) {
        diagram.setInput(this.name, val);
      }
    });
  };

  /**
   * Add an effect to run when the value of the input changes.
   * @param fn Effect to run
   */
  addEffect = (fn: (val: number) => void) => {
    this.valEffects.add(fn);
  };

  /**
   * Remove an effect added with `addEffect` from the input.
   * @param fn Effect to remove
   */
  removeEffect = (fn: (val: number) => void) => {
    this.valEffects.delete(fn);
  };

  /**
   * Get the current value of the input.
   */
  get = () => this.currVal;

  /**
   * Get whether the input is optimized.
   */
  getOptimized = () => this.optimized;

  private setValNoSyncing = (val: number) => {
    this.currVal = val;
    for (const effect of this.valEffects) {
      effect(val);
    }
  };

  private preventSyncing = (fn: () => void) => {
    this.syncing = true;
    fn();
    this.syncing = false;
  };

  private replaceEffects = () => {
    for (const [diagram, effect] of this.effectMap) {
      diagram.removeInputEffect(this.name, effect);
    }
    this.effectMap = new Map();
    for (const diagram of this.diagrams) {
      const effect = (val: number) => {
        if (this.syncing) return;
        this.preventSyncing(() => {
          this.setValNoSyncing(val);
          for (const otherDiagram of this.diagrams) {
            if (otherDiagram === diagram) continue;
            otherDiagram.setInput(this.name, val);
          }
        });
      };
      diagram.addInputEffect(this.name, effect);
      this.effectMap.set(diagram, effect);
    }
  };

  register = (diagram: Diagram) => {
    this.diagrams.add(diagram);
    this.replaceEffects();

    if (this.currVal === null) {
      // this must be the first diagram, so let's set the value to whatever
      // the diagram initialized it to
      this.currVal = diagram.getInput(this.name);
    }

    // performance: we don't need to sync the changes to other diagrams, since
    // it's the current value. Using `preventSyncing` will prevent
    // the syncing effects from running
    this.preventSyncing(() => diagram.setInput(this.name, this.currVal!));
  };

  unregister = (diagram: Diagram) => {
    this.diagrams.delete(diagram);
    this.replaceEffects();
  };
}

/**
 * Construct a diagram with a Penrose-like API. You may find it useful to
 * destructure an object of this type:
 *
 * ```TS
 * const {
 *   type,
 *   predicate,
 *   circle,
 *   line,
 *   ensure,
 *   // ...
 * } = new DiagramBuilder(canvas(400, 400), "seed");
 * ```
 */
export class DiagramBuilder {
  private substanceTypeMap: Map<Substance, Type> = new Map();
  private nextSubstanceId = 0;
  private substanceIdMap: Map<Substance, number> = new Map();
  private canvas: Canvas;
  private inputs: InputInfo[] = [];
  private varInputMap: Map<Var, number> = new Map();
  private namedInputs: Map<string, number> = new Map();
  private samplingContext: NamedSamplingContext;
  private shapes: Shape[] = [];
  private constraints: Num[] = [];
  private objectives: Num[] = [];
  private variation: string;
  private nextId = 0;
  private partialLayering: [string, string][] = [];
  private pinnedInputs: Set<number> = new Set();
  private dragNamesAndConstrs: Map<string, DragConstraint> = new Map();
  private externalInputs: Set<SharedInput> = new Set();
  private lassoStrength: number;
  private interactiveOnlyShapes: Set<Shape> = new Set();
  /** Shape name -> event name -> listener */
  private eventListeners: Map<
    string,
    [string, (e: any, diagram: Diagram) => void][]
  > = new Map();

  /**
   * Create a new diagram builder.
   * @param canvas Local dimensions of the SVG. This has no effect on the rendered size of your diagram--only the size
   *   of the local coordinate system.
   * @param variation Randomness seed
   * @param lassoStrength Strength of the optimizers lasso term. Higher values encourage diagram continuity,
   *   while lower values encourage reactivity. Default is 0.
   */
  constructor(
    canvas: Canvas,
    variation: string = "",
    lassoStrength: number = 0,
  ) {
    this.canvas = canvas;
    this.variation = variation;
    this.lassoStrength = lassoStrength;

    const { makeInput: createVar } = simpleContext(variation);
    this.samplingContext = {
      makeInput: (meta, name?) => {
        const newVar = createVar(meta);
        this.inputs.push({ handle: newVar, meta });
        if (name !== undefined) {
          if (this.namedInputs.has(name)) {
            throw new Error(`Duplicate input name ${name}`);
          }
          this.namedInputs.set(name, this.inputs.length - 1);
        }
        this.varInputMap.set(newVar, this.inputs.length - 1);
        return newVar;
      },
    };

    this.defineShapeMethods();

    this.input({ name: "_time", init: 0, optimized: false });
  }

  /**
   * Fill all shape methods
   */
  private defineShapeMethods = () => {
    const firstLetterLower = (s: string) => {
      return s.charAt(0).toLowerCase() + s.slice(1);
    };

    const shapeTypes = Object.keys(ShapeType) as ShapeType[];
    for (const shapeType of shapeTypes) {
      Object.defineProperty(this, firstLetterLower(shapeType), {
        value: (props: Partial<ShapeProps> = {}): Shape => {
          if (!("name" in props)) {
            props.name = String(this.nextId++) + "_" + shapeType;
          }

          const sampledPenroseShapeProps = sampleShape(
            shapeType,
            this.samplingContext,
            this.canvas,
          );

          // hacky: technically doesn't have path. But we never use it : |
          const sampledPenroseShape: PenroseShape<Num> = {
            ...sampledPenroseShapeProps,
            shapeType,
            passthrough: new Map(),
          } as PenroseShape<Num>;

          const shape = fromPenroseShape(sampledPenroseShape, props);

          if (shapeType === ShapeType.Group) {
            const group = shape as Group;
            const elements = new Set(group.shapes);
            if (group.clipPath) elements.add(group.clipPath);
            this.shapes = this.shapes.filter((s) => !elements.has(s));
          }

          if (shape.interactiveOnly) {
            this.interactiveOnlyShapes.add(shape);
          }
          this.shapes.push(shape);
          return shape;
        },
      });
    }
  };

  /**
   * Instantiate a new substance. If no type is given, this is identical to creating
   * and empty object. If a type `T` is given, this method is equivalent to calling `T()`.
   * @param type Type to instantiate
   */
  substance = (type?: Type): Substance => {
    const newSubstance: Substance = {};
    if (type) {
      this.substanceTypeMap.set(newSubstance, type);
    }
    this.substanceIdMap.set(newSubstance, this.nextSubstanceId++);
    return newSubstance;
  };

  /**
   * Create a new substance type. The type object serves as a constructor for new
   * substances:
   *
   * ```TS
   * const Vector = type();
   *
   * const v1 = Vector();
   * const v2 = Vector();
   * ...
   * ```
   */
  type = (): Type => {
    const substance = this.substance.bind(this);
    return function t() {
      return substance(t);
    };
  };

  /**
   * Create a new predicate over substances.
   *
   * The returned predicate function can be used to declare relationships
   * between substances (by calling the predicate) and to query existing relation
   * (by calling the predicate's `.test` method):
   *
   *  ```TS
   *  const Vector = type();
   *  const Orthogonal = predicate();
   *
   *  const v1 = Vector();
   *  const v2 = Vector();
   *
   *  Orthogonal.test(v1, v2); // returns false
   *  Orthogonal(v1, v2);
   *  Orthogonal.test(v1, v2); // returns true
   *  ```
   */
  predicate = (): Predicate => {
    const objSeqs: any[][] = [];
    const pred: Partial<Predicate> = (...objs: any[]) => {
      objSeqs.push(objs);
    };
    pred.test = (...objs: any[]) =>
      objSeqs.some((realObjs) => {
        return (
          realObjs.length === objs.length &&
          realObjs.every((o, i) => o === objs[i])
        );
      });

    return pred as Predicate;
  };

  private internalForallWhere = (
    vars: SelectorVars,
    deduplicate: boolean,
    where: (assigned: SelectorAssignment) => boolean,
    func: (assigned: SelectorAssignment, matchId: number) => void,
  ) => {
    const visited = new Set<string>();

    const matchString = (assigned: SelectorAssignment) => {
      return Object.values(assigned)
        .map((s) => this.substanceIdMap.get(s)!)
        .sort()
        .join("-");
    };

    const traverse = (
      pairsToAssign: { name: string; type: Type }[],
      assignment: { name: string; substance: Substance }[],
      matches: number,
    ) => {
      if (pairsToAssign.length === 0) return 0;

      for (const [subst, type] of this.substanceTypeMap) {
        if (
          pairsToAssign[0].type === type &&
          !assignment.some((a) => a.substance === subst)
        ) {
          if (pairsToAssign.length === 1) {
            const assignmentRecord: SelectorAssignment = {};
            for (const { name, substance: assignedSubst } of assignment) {
              assignmentRecord[name] = assignedSubst;
            }
            assignmentRecord[pairsToAssign[0].name] = subst;
            if (where(assignmentRecord)) {
              const str = matchString(assignmentRecord);
              if (!deduplicate || !visited.has(str)) {
                func(assignmentRecord, matches++);
                if (deduplicate) {
                  visited.add(str);
                }
              }
            }
          } else {
            const newAssignment = [
              ...assignment,
              { name: pairsToAssign[0].name, substance: subst },
            ];
            const newPairsToAssign = pairsToAssign.slice(1);
            matches = traverse(newPairsToAssign, newAssignment, matches);
          }
        }
      }

      return matches;
    };

    traverse(
      Object.entries(vars).map(([name, type]) => ({ name, type })),
      [],
      0,
    );
  };

  /**
   * Iterate over all possible assignments of substances to variables, subject to condition `where`.
   * This selection does NOT deduplicate assignments.
   * @param vars
   * @param where
   * @param func
   */
  forallWhere = (
    vars: SelectorVars,
    where: (assigned: SelectorAssignment) => boolean,
    func: (assigned: SelectorAssignment, matchId: number) => void,
  ) => {
    return this.internalForallWhere(vars, false, where, func);
  };

  /**
   * Iterate over all possible assignments of substances to variables.
   * This selection deduplicates assignments.
   * @param vars
   * @param func
   */
  forall = (
    vars: SelectorVars,
    func: (assigned: SelectorAssignment, matchId: number) => void,
  ) => {
    this.internalForallWhere(vars, true, () => true, func);
  };

  /**
   * Create a new input.
   * @param info `InputOpts` for the input
   */
  input = (info?: InputOpts): Var => {
    const name = info?.name;
    const init = info?.init;
    const pinned = !(info?.optimized ?? true);

    const newVar = this.samplingContext.makeInput(
      {
        init: {
          tag: "Sampled",
          sampler:
            init !== undefined ? () => init : uniform(...this.canvas.xRange),
        },
        stages: "All", // no staging for now
      },
      name,
    );

    if (pinned) {
      this.pinnedInputs.add(this.inputs.length - 1);
    }
    return newVar;
  };

  /**
   * Create an input from a `SharedInput`.
   * @param input `SharedInput` to create an input from
   * @param initOverride Override the initial value of the shared input
   */
  sharedInput = (input: SharedInput, initOverride?: number) => {
    this.externalInputs.add(input);
    return this.input({
      name: input.name,
      init: initOverride ?? input.init,
      optimized: input.getOptimized(),
    });
  };

  // for typing only; dynamically generated
  /* eslint-disable @typescript-eslint/no-unused-vars */
  /** Create a new circle. Options: see https://penrose.cs.cmu.edu/docs/ref/style/shapes/circle */
  circle = (_props: Partial<CircleProps> = {}): Readonly<Circle> => {
    throw new Error("Not filled");
  };
  /** Create a new ellipse. Options: see https://penrose.cs.cmu.edu/docs/ref/style/shapes/ellipse */
  ellipse = (_props: Partial<EllipseProps> = {}): Readonly<Ellipse> => {
    throw new Error("Not filled");
  };
  /** Create a new equation. Options: see https://penrose.cs.cmu.edu/docs/ref/style/shapes/equation */
  equation = (_props: Partial<EquationProps> = {}): Readonly<Equation> => {
    throw new Error("Not filled");
  };
  /** Create a new image. Options: see https://penrose.cs.cmu.edu/docs/ref/style/shapes/image */
  image = (_props: Partial<ImageProps> = {}): Readonly<Image> => {
    throw new Error("Not filled");
  };
  /** Create a new line. Options: see https://penrose.cs.cmu.edu/docs/ref/style/shapes/line */
  line = (_props: Partial<LineProps> = {}): Readonly<Line> => {
    throw new Error("Not filled");
  };
  /** Create a new path. Options: see https://penrose.cs.cmu.edu/docs/ref/style */
  path = (_props: Partial<PathProps> = {}): Readonly<Path> => {
    throw new Error("Not filled");
  };
  /** Create a new polygon. Options: see https://penrose.cs.cmu.edu/docs/ref/style/shapes/polygon */
  polygon = (_props: Partial<PolygonProps> = {}): Readonly<Polygon> => {
    throw new Error("Not filled");
  };
  /** Create a new polyline. Options: see https://penrose.cs.cmu.edu/docs/ref/style/shapes/polyline */
  polyline = (_props: Partial<PolylineProps> = {}): Readonly<Polyline> => {
    throw new Error("Not filled");
  };
  /** Create a new rectangle. Options: see https://penrose.cs.cmu.edu/docs/ref/style/shapes/rectangle */
  rectangle = (_props: Partial<RectangleProps> = {}): Readonly<Rectangle> => {
    throw new Error("Not filled");
  };
  /** Create a new text. Options: see https://penrose.cs.cmu.edu/docs/ref/style/shapes/text */
  text = (_props: Partial<TextProps> = {}): Readonly<Text> => {
    throw new Error("Not filled");
  };
  /** Create a new group. Options: see https://penrose.cs.cmu.edu/docs/ref/style/shapes/group
   * Note that, unlike in Penrose, the `clipPath` field takes either a `Shape` or `null`. */
  group = (_props: Partial<GroupProps> = {}): Readonly<Group> => {
    throw new Error("Not filled");
  };
  /* eslint-enable @typescript-eslint/no-unused-vars */

  /**
   * Register a constraint with the diagram.
   * @param constraint A `Num` created with the `constraints` module (or, if you know what you're doing, by yourself.
   *   You can read about creating custom constaints at https://penrose.cs.cmu.edu/docs/ref/constraints)
   * @param weight An optional weight to multiply the constraint by. If you find that your constraints are not
   *   being satisfied, you may want to try increasing the weight.
   */
  ensure = (constraint: Num, weight?: number) => {
    if (weight) {
      constraint = mul(constraint, weight);
    }
    this.constraints.push(constraint);
  };

  /**
   * Register an objective with the diagram.
   *
   * @param objective A `Num` created with the `objectives` module (or, if you know what you're doing, by yourself.
   *   You can read about creating custom constaints at https://penrose.cs.cmu.edu/docs/ref/constraints)
   * @param weight An optional weight to multiply the objective by. If you find that your objectives are not
   *   being satisfied, you may want to try increasing the weight.
   */
  encourage = (objective: Num, weight?: number) => {
    if (weight) {
      objective = mul(objective, weight);
    }
    this.constraints.push(objective);
  };

  /**
   * Require that one shape be above another.
   * @param below The bottom shape
   * @param above The top shape
   */
  layer = (below: Shape, above: Shape) => {
    this.partialLayering.push([below.name, above.name]);
  };

  /**
   * An input giving the current time since the diagram was mounted. You can use this
   * to create animations. However, you _must_ use the `AnimatedRenderer` component to
   * render the diagram, otherwise this input will not be updated.
   */
  time = () => {
    return this.getInput("_time");
  };

  /**
   * Get an input by name.
   * @param name Name of the input
   */
  getInput = (name: string) => {
    const idx = this.namedInputs.get(name);
    if (idx === undefined) {
      throw new Error(`No input named ${name}`);
    }
    return this.inputs[idx].handle;
  };

  /**
   * Build the diagram.
   */
  build = async (): Promise<Diagram> => {
    this.addDragConstraints();
    this.addOnCanvasConstraints();

    if (this.lassoStrength !== 0) {
      this.makeLassoInputs();
    }

    const nameShapeMap = this.getNameShapeMap();
    const interactiveOnlyShapes = new Set<PenroseShape<Num>>();
    const eventListeners = new Map<
      string,
      [string, (e: any, diagram: Diagram) => void][]
    >();
    const penroseShapes = this.shapes.map((s) => {
      const penroseShape = toPenroseShape(s);
      if (this.interactiveOnlyShapes.has(s)) {
        interactiveOnlyShapes.add(penroseShape);
      }
      if (this.eventListeners.has(s.name)) {
        if (!eventListeners.has(penroseShape.name.contents)) {
          eventListeners.set(penroseShape.name.contents, []);
        }
        const inner = eventListeners.get(penroseShape.name.contents)!;
        for (const [event, listener] of this.eventListeners.get(s.name)!) {
          inner.push([event, listener]);
        }
      }
      return penroseShape;
    });
    const orderedShapes = sortShapes(penroseShapes, this.partialLayering);

    const diagram = await Diagram.create({
      canvas: this.canvas,
      variation: this.variation,
      inputs: this.inputs,
      constraints: this.constraints,
      objectives: this.objectives,
      shapes: orderedShapes,
      nameShapeMap,
      namedInputs: this.namedInputs,
      pinnedInputs: this.pinnedInputs,
      draggingConstraints: this.dragNamesAndConstrs,
      inputIdxsByPath: this.getTranslatedInputIdxsByPath(),
      lassoStrength: this.lassoStrength,
      sharedInputs: this.externalInputs,
      interactiveOnlyShapes,
      eventListeners,
    });

    for (const input of this.externalInputs) {
      input.register(diagram);
    }

    return diagram;
  };

  /**
   * Create a new input, and constrain that input to a calculated value.
   * This is useful for reading calculated values from the diagram using `getInput`,
   * and for creating draggable values.
   * @param num Value to bind the input to
   * @param info `InputOpts` for the input
   */
  bindToInput = (num: Num, info?: InputOpts) => {
    const inp = this.input(info);
    this.ensure(constraints.equal(inp, num));
    return inp;
  };

  addEventListener = (
    shape: Shape,
    event: string,
    listener: (e: any, diagram: Diagram) => void,
  ) => {
    let inner: [string, (e: any, diagram: Diagram) => void][];
    if (this.eventListeners.has(shape.name)) {
      inner = this.eventListeners.get(shape.name)!;
    } else {
      inner = [];
      this.eventListeners.set(shape.name, inner);
    }

    inner.push([event, listener]);
  };

  private addOnCanvasConstraints = () => {
    for (const shape of this.shapes) {
      if (shape.ensureOnCanvas) {
        this.ensure(
          constraints.onCanvas(shape, this.canvas.width, this.canvas.height),
        );
      }
    }
  };

  private addDragConstraints = () => {
    const addDragConstraint = (shape: Drag & Shape) => {
      if (shape.dragConstraint) {
        this.dragNamesAndConstrs.set(shape.name, shape.dragConstraint);
      } else {
        this.dragNamesAndConstrs.set(shape.name, ([x, y]) => [x, y]);
      }
    };

    for (const shape of this.shapes) {
      if (shape.shapeType === ShapeType.Group) {
        for (const s of (shape as Group).shapes) {
          if ("drag" in s && s.drag) {
            addDragConstraint(s);
          }
        }
      } else if ("drag" in shape && shape.drag) {
        addDragConstraint(shape);
      }
    }
  };

  private getTranslatedInputIdxsByPath = () => {
    const mapNum = (num: Num) => {
      if (isVar(num)) {
        return this.varInputMap.get(num);
      }
      return undefined;
    };

    const mapVec2 = (vec: Vec2): any => {
      return {
        tag: "Val",
        contents: {
          tag: "VectorV",
          contents: vec.map(mapNum),
        },
      };
    };

    const inputIdxsByPath: IdxsByPath = new Map();
    const applyShape = (shape: Shape) => {
      if ("drag" in shape && shape.drag) {
        if ("center" in shape) {
          const idxs = mapVec2(shape.center);
          inputIdxsByPath.set(shape.name + ".center", idxs);
        }

        if ("start" in shape) {
          const idxs = mapVec2(shape.start);
          inputIdxsByPath.set(shape.name + ".start", idxs);
        }

        if ("end" in shape) {
          const idxs = mapVec2(shape.end);
          inputIdxsByPath.set(shape.name + ".end", idxs);
        }

        if ("points" in shape) {
          const idxs = {
            tag: "Val",
            contents: {
              tag: "LListV",
              contents: shape.points.map((v) => v.map(mapNum)),
            },
          };
          inputIdxsByPath.set(shape.name + ".points", idxs as any);
        }
      }

      if (shape.shapeType === ShapeType.Group) {
        shape.shapes.map(applyShape);
      }
    };

    this.shapes.map(applyShape);

    return inputIdxsByPath;
  };

  private getNameShapeMap = () => {
    const nameShapeMap = new Map<string, PenroseShape<Num>>();
    for (const s of this.shapes) {
      nameShapeMap.set(s.name, toPenroseShape(s));
      if (s.shapeType === ShapeType.Group) {
        s.shapes.map((subshape) =>
          nameShapeMap.set(subshape.name, toPenroseShape(subshape)),
        );
        if (s.clipPath) {
          nameShapeMap.set(s.clipPath.name, toPenroseShape(s.clipPath));
        }
      }
    }
    return nameShapeMap;
  };

  private makeLassoInputs = () => {
    this.inputs.map((i) =>
      this.input({ init: i.handle.val, optimized: false }),
    );
  };
}
