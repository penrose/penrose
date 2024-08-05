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
import constraints from "./constraints.js";
import { Diagram } from "./diagram.js";
import {
  Circle,
  CircleProps,
  DragConstraint,
  Ellipse,
  EllipseProps,
  Equation,
  EquationProps,
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

export type NamedSamplingContext = {
  makeInput: (meta: InputMeta, name?: string) => Var;
};

export type SelectorVars = Record<string, Type>;
export type SelectorAssignment = Record<string, Substance>;

export type InputOpts = {
  name?: string;
  init?: number;
  pinned?: boolean;
};

export class SharedInput {
  public readonly name: string;
  public readonly init?: number;

  private diagrams = new Set<Diagram>();
  private effectMap = new Map<Diagram, (val: number) => void>();
  private currVal: number | null = null;
  private syncing: boolean = false;
  private static nextId = 0;

  constructor(name?: string, init?: number) {
    this.name = name ?? `_input_${SharedInput.nextId++}`;
    this.init = init;
  }

  set = (val: number) => {
    this.currVal = val;
    this.preventSyncing(() => {
      for (const diagram of this.diagrams) {
        diagram.setInput(this.name, val);
      }
    });
  };

  get = () => this.currVal;

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
          this.currVal = val;
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
    // it's the current value. Using `withSyncing` will prevent
    // the syncing effects from running
    this.preventSyncing(() => diagram.setInput(this.name, this.currVal!));
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

  /**
   * Create a new diagram builder.
   * @param canvas Local dimensions of the SVG. If you find you need thinner lines, try increasing these dimensions.
   * @param variation Randomness seed
   */
  constructor(canvas: Canvas, variation: string, lassoStrength: number = 0) {
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

    this.input({ name: "_time", init: 0, pinned: true });
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

  input = (info?: InputOpts): Var => {
    const name = info?.name;
    const init = info?.init;
    const pinned = info?.pinned ?? false;

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

  sharedInput = (input: SharedInput, initOverride?: number) => {
    this.externalInputs.add(input);
    return this.input({
      name: input.name,
      init: initOverride ?? input.init,
      pinned: true,
    });
  };

  // for typing only; dynamically generated
  /* eslint-disable @typescript-eslint/no-unused-vars */
  circle = (_props: Partial<CircleProps> = {}): Readonly<Circle> => {
    throw new Error("Not filled");
  };
  ellipse = (_props: Partial<EllipseProps> = {}): Readonly<Ellipse> => {
    throw new Error("Not filled");
  };
  equation = (_props: Partial<EquationProps> = {}): Readonly<Equation> => {
    throw new Error("Not filled");
  };
  image = (_props: Partial<ImageProps> = {}): Readonly<Image> => {
    throw new Error("Not filled");
  };
  line = (_props: Partial<LineProps> = {}): Readonly<Line> => {
    throw new Error("Not filled");
  };
  path = (_props: Partial<PathProps> = {}): Readonly<Path> => {
    throw new Error("Not filled");
  };
  polygon = (_props: Partial<PolygonProps> = {}): Readonly<Polygon> => {
    throw new Error("Not filled");
  };
  polyline = (_props: Partial<PolylineProps> = {}): Readonly<Polyline> => {
    throw new Error("Not filled");
  };
  rectangle = (_props: Partial<RectangleProps> = {}): Readonly<Rectangle> => {
    throw new Error("Not filled");
  };
  text = (_props: Partial<TextProps> = {}): Readonly<Text> => {
    throw new Error("Not filled");
  };
  /* eslint-enable @typescript-eslint/no-unused-vars */

  ensure = (constraint: Num, weight?: number) => {
    if (weight) {
      constraint = mul(constraint, weight);
    }
    this.constraints.push(constraint);
  };

  encourage = (objective: Num, weight?: number) => {
    if (weight) {
      objective = mul(objective, weight);
    }
    this.constraints.push(objective);
  };

  layer = (below: Shape, above: Shape) => {
    this.partialLayering.push([below.name, above.name]);
  };

  time = () => {
    return this.getInput("_time");
  };

  getInput = (name: string) => {
    const idx = this.namedInputs.get(name);
    if (idx === undefined) {
      throw new Error(`No input named ${name}`);
    }
    return this.inputs[idx].handle;
  };

  build = async (): Promise<Diagram> => {
    this.addDragConstraints();
    this.addOnCanvasConstraints();

    if (this.lassoStrength !== 0) {
      this.makeLassoInputs();
    }

    const nameShapeMap = this.getNameShapeMap();
    const penroseShapes = this.shapes.map((s) => toPenroseShape(s));
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
      dragNamesAndConstrs: this.dragNamesAndConstrs,
      inputIdxsByPath: this.getTranslatedInputIdxsByPath(),
      lassoStrength: this.lassoStrength,
    });

    for (const input of this.externalInputs) {
      input.register(diagram);
    }

    return diagram;
  };

  bindToInput = (num: Num, info?: InputOpts) => {
    const inp = this.input(info);
    this.ensure(constraints.equal(inp, num));
    return inp;
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
    for (const shape of this.shapes) {
      if ("drag" in shape && shape.drag) {
        if (shape.dragConstraint) {
          this.dragNamesAndConstrs.set(shape.name, shape.dragConstraint);
        } else {
          this.dragNamesAndConstrs.set(shape.name, ([x, y]) => [x, y]);
        }
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
    for (const shape of this.shapes) {
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
    }

    return inputIdxsByPath;
  };

  private getNameShapeMap = () => {
    const nameShapeMap = new Map<string, PenroseShape<Num>>();
    for (const s of this.shapes) {
      nameShapeMap.set(s.name, toPenroseShape(s));
    }
    return nameShapeMap;
  };

  private makeLassoInputs = () => {
    this.inputs.map((i) => this.input({ init: i.handle.val, pinned: true }));
  };
}
