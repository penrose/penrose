import {
  Canvas,
  Graph,
  IdxsByPath,
  InputInfo,
  InputMeta,
  Num,
  Shape as PenroseShape,
  Var,
  add,
  isVar,
  mul,
  sampleShape,
  simpleContext,
  sub,
  uniform,
} from "@penrose/core";
import * as constraints from "./constraints.js";
import { Diagram } from "./diagram.js";
import * as objectives from "./objectives.js";
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
  RawSvgElement,
  Rectangle,
  RectangleProps,
  Shape,
  ShapeProps,
  ShapeType,
  Substance,
  Text,
  TextProps,
  Type,
  Vec2,
} from "./types.js";
import { fromPenroseShape, sortShapes, toPenroseShape } from "./utils.js";

type NamedSamplingContext = {
  makeInput: (meta: InputMeta, name?: string) => Var;
};

type AffineCoordinate = { offset: number; coefficients: Map<Var, number> };

/** A single-axis affine coordinate can be translated without changing its shape. */
const affineCoordinate = (num: Num): AffineCoordinate | undefined => {
  if (typeof num === "number") {
    return Number.isFinite(num)
      ? { offset: num, coefficients: new Map() }
      : undefined;
  }
  if (isVar(num)) return { offset: 0, coefficients: new Map([[num, 1]]) };
  const scaled = (
    value: AffineCoordinate,
    factor: number,
  ): AffineCoordinate => ({
    offset: value.offset * factor,
    coefficients: new Map(
      [...value.coefficients]
        .map(([v, coefficient]): [Var, number] => [v, coefficient * factor])
        .filter(([, coefficient]) => coefficient !== 0),
    ),
  });
  if (num.tag === "Unary" && num.unop === "neg") {
    const value = affineCoordinate(num.param);
    return value && scaled(value, -1);
  }
  if (num.tag !== "Binary") return undefined;
  const left = affineCoordinate(num.left);
  const right = affineCoordinate(num.right);
  if (!left || !right) return undefined;
  if (num.binop === "+" || num.binop === "-") {
    const result = scaled(right, num.binop === "+" ? 1 : -1);
    result.offset += left.offset;
    for (const [v, coefficient] of left.coefficients) {
      const sum = coefficient + (result.coefficients.get(v) ?? 0);
      if (sum === 0) result.coefficients.delete(v);
      else result.coefficients.set(v, sum);
    }
    return result;
  }
  if (num.binop === "*") {
    if (left.coefficients.size === 0) return scaled(right, left.offset);
    if (right.coefficients.size === 0) return scaled(left, right.offset);
  }
  if (
    num.binop === "/" &&
    right.coefficients.size === 0 &&
    right.offset !== 0
  ) {
    return scaled(left, 1 / right.offset);
  }
  return undefined;
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

/** Opt-in layout exploration; mathematical shape coordinates remain unchanged by default. */
export interface InteractiveLayoutOptions {
  /** Initial seed-dependent displacement in diagram units. */
  jitter?: number;
  /** Maximum displacement from the source layout while dragging. */
  maxDistance?: number;
}

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
  /** Subtype → supertype edges; same orientation as Domain's type graph. */
  private typeGraph: Graph<Type> = new Graph();
  private nextSubstanceId = 0;
  private substanceIdMap: Map<Substance, number> = new Map();
  private canvas: Canvas;
  private inputs: InputInfo[] = [];
  private varInputMap: Map<Var, number> = new Map();
  private namedInputs: Map<string, number> = new Map();
  private samplingContext: NamedSamplingContext;
  private shapes: Shape[] = [];
  private constraints: Num[] = [];
  private constraintNames: (string | undefined)[] = [];
  private objectives: Num[] = [];
  private variation: string;
  private nextId = 0;
  private partialLayering: [string, string][] = [];
  private pinnedInputs: Set<number> = new Set();
  private externalInputs: Set<SharedInput> = new Set();
  private lassoStrength: number;
  private rawSvgDefs: RawSvgElement[] = [];
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
    setActiveBuilder(this);

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

  /** Convert SVG coordinates (top-left origin, y down) to Penrose coordinates. */
  svgPoint = ([x, y]: Vec2): Vec2 => [
    sub(x, this.canvas.width / 2),
    sub(this.canvas.height / 2, y),
  ];

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

          this.shapes.push(shape);
          return shape;
        },
      });
    }
  };

  /**
   * Register a raw SVG element (defs, linearGradient, etc.) to be injected into
   * the rendered SVG. Called automatically by the JSX factory for unknown element types.
   * When a parent element is added, its children are removed from the top-level list.
   */
  addRawSvgDef = (def: RawSvgElement) => {
    // Remove any existing registered elements that are children of this new element
    const descendants = this.getAllRawSvgDescendants(def);
    this.rawSvgDefs = this.rawSvgDefs.filter((d) => !descendants.has(d));
    this.rawSvgDefs.push(def);
  };

  private getAllRawSvgDescendants = (
    def: RawSvgElement,
  ): Set<RawSvgElement> => {
    const result = new Set<RawSvgElement>();
    for (const child of def.children) {
      result.add(child);
      for (const desc of this.getAllRawSvgDescendants(child)) {
        result.add(desc);
      }
    }
    return result;
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
   * substances. Pass previously declared types as arguments to make them direct
   * supertypes of the new type.
   *
   * ```TS
   * const Vector = type();
   * const Drawable = type();
   * const UnitVector = type(Vector, Drawable);
   *
   * const v1 = Vector();
   * const v2 = UnitVector();
   * ```
   *
   * A selector for `Vector` matches both `v1` and `v2`. Supertypes must be
   * created by the same `DiagramBuilder` before their subtypes.
   *
   * @param supertypes Types that the new type extends
   */
  type = (...supertypes: Type[]): Type => {
    for (const supertype of supertypes) {
      if (!this.typeGraph.hasNode(supertype)) {
        throw new Error(
          "Supertypes must be created by the same DiagramBuilder before their subtypes",
        );
      }
    }

    const substance = this.substance.bind(this);
    const t = function t() {
      return substance(t);
    };
    this.typeGraph.setNode(t, undefined);
    // Deduplicate so repeated direct parents (e.g. type(A, A)) don't add multi-edges.
    for (const supertype of new Set(supertypes)) {
      this.typeGraph.setEdge({ i: t, j: supertype, e: undefined });
    }
    return t;
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

  private isSubtype = (actual: Type, expected: Type): boolean => {
    // Matches Domain.isDeclaredSubtype / superTypesOf via Graph.descendants.
    return this.typeGraph.descendants(actual).has(expected);
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
          this.isSubtype(type, pairsToAssign[0].type) &&
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
                withBuilder(this, () => {
                  func(assignmentRecord, matches++);
                });
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
   * @param label An optional explanation used by `Diagram.getConstraintDiagnostics`.
   */
  ensure = (constraint: Num, weight?: number, label?: string) => {
    if (weight !== undefined) {
      constraint = mul(constraint, weight);
    }
    this.constraints.push(constraint);
    this.constraintNames.push(label);
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
    if (weight !== undefined) {
      objective = mul(objective, weight);
    }
    this.objectives.push(objective);
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
   * Move a fixed-coordinate construction through shared native Bloom inputs.
   * The handle and companions translate together, preserving their geometry.
   * Styles can opt into geometric groups; label layouts use the same mechanism.
   */
  draggableGroup = (
    handle: Shape & {
      center: Vec2;
      drag: boolean;
      dragConstraint: DragConstraint;
    },
    companions: readonly Shape[] = [],
    options: InteractiveLayoutOptions = {},
  ) => {
    const jitter = options.jitter ?? 4;
    const maxDistance = options.maxDistance ?? 16;
    if (
      !(jitter >= 0 && maxDistance > 0 && maxDistance >= jitter) ||
      ![jitter, maxDistance].every(Number.isFinite)
    )
      throw new Error(
        "Interactive jitter must be nonnegative and bounded by a positive drag distance",
      );
    const original = [...handle.center] as Vec2;
    if (!original.every((n) => typeof n === "number" && Number.isFinite(n)))
      throw new Error("A native drag group requires a fixed-coordinate handle");
    const sampled = (axis: string, pinned: boolean) => {
      const variable = this.samplingContext.makeInput(
        {
          init: {
            tag: "Sampled",
            sampler: jitter === 0 ? () => 0 : uniform(-jitter, jitter),
          },
          stages: "All",
        },
        `${handle.name}.layout.${axis}`,
      );
      if (pinned) this.pinnedInputs.add(this.inputs.length - 1);
      return variable;
    };
    const dx = sampled("x", false),
      dy = sampled("y", false);
    const targetX = sampled("anchor-x", true),
      targetY = sampled("anchor-y", true);
    this.encourage(objectives.equal(dx, targetX), 0.1);
    this.encourage(objectives.equal(dy, targetY), 0.1);
    this.ensure(constraints.inRange(dx, -maxDistance, maxDistance));
    this.ensure(constraints.inRange(dy, -maxDistance, maxDistance));
    const moved = new Set<Shape>();
    const translate = (shape: Shape) => {
      if (moved.has(shape)) return;
      moved.add(shape);
      const point = ([x, y]: Vec2): Vec2 => [add(x, dx), add(y, dy)];
      if ("center" in shape) shape.center = point(shape.center);
      if ("start" in shape) shape.start = point(shape.start);
      if ("end" in shape) shape.end = point(shape.end);
      if ("points" in shape) shape.points = shape.points.map(point);
      if (shape.shapeType === ShapeType.Path) {
        shape.d = shape.d.map((command) => ({
          ...command,
          contents: command.contents.map((value) =>
            value.tag === "CoordV"
              ? { ...value, contents: point(value.contents as Vec2) }
              : value,
          ),
        }));
      }
      if (shape.shapeType === ShapeType.Group) {
        shape.shapes.forEach(translate);
        if (shape.clipPath) translate(shape.clipPath);
      }
    };
    [handle, ...companions].forEach(translate);
    handle.drag = true;
    const [cx, cy] = original as [number, number];
    handle.dragConstraint = ([x, y]) => [
      Math.min(cx + maxDistance, Math.max(cx - maxDistance, x)),
      Math.min(cy + maxDistance, Math.max(cy - maxDistance, y)),
    ];
    handle.rawAttrs = {
      ...handle.rawAttrs,
      tabindex: "0",
      role: "button",
      cursor: "grab",
      "data-bloom-drag": "true",
      "aria-label":
        handle.rawAttrs?.["aria-label"] ??
        `Drag ${"string" in handle ? handle.string : handle.name}`,
      "aria-keyshortcuts": "ArrowUp ArrowDown ArrowLeft ArrowRight",
    };
    this.addEventListener(
      handle,
      "keydown",
      (event: KeyboardEvent, diagram) => {
        const direction: Record<string, [number, number]> = {
          ArrowLeft: [-1, 0],
          ArrowRight: [1, 0],
          ArrowUp: [0, 1],
          ArrowDown: [0, -1],
        };
        if (!direction[event.key]) return;
        event.preventDefault();
        event.stopPropagation();
        const distance = event.shiftKey ? 8 : 2;
        const currentX = diagram.getInput(`${handle.name}.layout.x`);
        const currentY = diagram.getInput(`${handle.name}.layout.y`);
        const [tx, ty] = handle.dragConstraint(
          [
            cx + currentX + distance * direction[event.key][0],
            cy + currentY + distance * direction[event.key][1],
          ],
          diagram,
        );
        diagram.beginDrag(handle.name);
        diagram.translate(handle.name, tx - cx - currentX, ty - cy - currentY);
        diagram.endDrag(handle.name);
      },
    );
    return { dx, dy };
  };

  /** Add seed-dependent label layouts, retaining exact mathematical geometry. */
  interactiveLabels = (options: InteractiveLayoutOptions = {}) => {
    const all: Shape[] = [];
    const seen = new Set<Shape>();
    const collect = (shape: Shape) => {
      if (seen.has(shape)) return;
      seen.add(shape);
      all.push(shape);
      if (shape.shapeType === ShapeType.Group) shape.shapes.forEach(collect);
    };
    this.shapes.forEach(collect);
    const labels = all.filter(
      (shape): shape is Equation | Text =>
        (shape.shapeType === ShapeType.Equation ||
          shape.shapeType === ShapeType.Text) &&
        !shape.drag &&
        shape.center.every((n) => typeof n === "number") &&
        !/^(?:[()[\]{}]|\\(?:smile|frown))$/.test(shape.string),
    );
    const points = all.filter(
      (shape): shape is Circle =>
        shape.shapeType === ShapeType.Circle &&
        typeof shape.r === "number" &&
        shape.r > 0 &&
        shape.r <= 6,
    );
    for (const label of labels) {
      const knockouts = all.filter(
        (shape): shape is Rectangle =>
          shape.shapeType === ShapeType.Rectangle &&
          shape.center[0] === label.center[0] &&
          shape.center[1] === label.center[1] &&
          typeof shape.height === "number" &&
          shape.height <= 80 &&
          typeof shape.width === "number" &&
          shape.width <= this.canvas.width / 2 &&
          shape.fillColor.every((n) => n === 1) &&
          shape.strokeWidth === 0,
      );
      this.draggableGroup(label, knockouts, options);
      if (options.jitter !== 0) {
        label.ensureOnCanvas = true;
        knockouts.forEach((shape) => {
          shape.ensureOnCanvas = true;
        });
        for (const point of points)
          this.ensure(constraints.disjoint(label, point, 1));
      } else {
        // The canonical composition may deliberately align type beyond the
        // drawing's bounds. Native handles must not reflow it on first mount.
        label.ensureOnCanvas = false;
        knockouts.forEach((shape) => {
          shape.ensureOnCanvas = false;
        });
      }
    }
    if (options.jitter !== 0)
      for (let i = 0; i < labels.length; i++)
        for (let j = i + 1; j < labels.length; j++)
          this.ensure(constraints.disjoint(labels[i], labels[j], 1));
  };

  /**
   * Build the diagram.
   */
  build = async (): Promise<Diagram> => {
    const dragNamesAndConstrs = this.getDragConstraints();
    const { inputIdxsByPath, dragInputScales } = this.getTranslationInputs();
    const onCanvasConstraints = this.getOnCanvasConstraints();

    const inputs =
      this.lassoStrength !== 0
        ? [...this.inputs, ...this.makeLassoInputs()]
        : [...this.inputs];
    const pinnedInputs = new Set(this.pinnedInputs);
    const constraints = [...this.constraints, ...onCanvasConstraints];
    const constraintNames = [
      ...this.constraintNames,
      ...this.shapes
        .filter((shape) => shape.ensureOnCanvas)
        .map((shape) => `onCanvas(${shape.name})`),
    ];
    const objectives = [...this.objectives];

    if (this.lassoStrength !== 0) {
      for (let i = 0; i < this.inputs.length; i++) {
        pinnedInputs.add(i + this.inputs.length);
      }
    }

    const nameShapeMap = this.getNameShapeMap();
    const interactiveOnlyShapes = new Set<PenroseShape<Num>>();
    const eventListeners = new Map<
      string,
      [string, (e: any, diagram: Diagram) => void][]
    >();
    // Collect rawAttrs from Bloom shapes (for post-render SVG attribute overrides)
    const rawAttrsByName = new Map<string, Record<string, string>>();
    const collectMetadata = (shape: Shape, penroseShape: PenroseShape<Num>) => {
      if (shape.rawAttrs && Object.keys(shape.rawAttrs).length > 0) {
        rawAttrsByName.set(shape.name, { ...shape.rawAttrs });
      }
      if (shape.interactiveOnly) interactiveOnlyShapes.add(penroseShape);
      if (this.eventListeners.has(shape.name)) {
        eventListeners.set(shape.name, [
          ...this.eventListeners.get(shape.name)!,
        ]);
      }
      if (shape.shapeType === ShapeType.Group) {
        const group = penroseShape as Extract<
          PenroseShape<Num>,
          { shapeType: "Group" }
        >;
        shape.shapes.forEach((child, i) =>
          collectMetadata(child, group.shapes.contents[i]),
        );
        if (shape.clipPath && group.clipPath.contents.tag === "Clip") {
          collectMetadata(shape.clipPath, group.clipPath.contents.contents);
        }
      }
    };
    const penroseShapes = this.shapes.map((s) => {
      const penroseShape = toPenroseShape(s);
      collectMetadata(s, penroseShape);
      return penroseShape;
    });
    const orderedShapes = sortShapes(penroseShapes, this.partialLayering);

    const diagram = await Diagram.create({
      canvas: this.canvas,
      variation: this.variation,
      inputs,
      constraints,
      constraintNames,
      objectives,
      shapes: orderedShapes,
      nameShapeMap,
      namedInputs: new Map(this.namedInputs),
      pinnedInputs: new Set(this.pinnedInputs),
      draggingConstraints: new Map(dragNamesAndConstrs),
      rawSvgDefs: [...this.rawSvgDefs],
      rawAttrsByName,
      inputIdxsByPath,
      dragInputScales,
      lassoStrength: this.lassoStrength,
      sharedInputs: new Set(this.externalInputs),
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

  removeEventListener = (
    shape: Shape,
    event: string,
    listener: (e: any, diagram: Diagram) => void,
  ) => {
    if (!this.eventListeners.has(shape.name)) return;
    const inner = this.eventListeners.get(shape.name)!;
    const idx = inner.findIndex(([e, l]) => e === event && l === listener);
    if (idx === -1) return;
    inner.splice(idx, 1);
  };

  setOptimized = (name: string, optimized: boolean) => {
    if (!this.namedInputs.has(name)) {
      throw new Error("No named input with name '" + name + "'");
    }

    const ipt = this.namedInputs.get(name)!;

    if (optimized) {
      this.pinnedInputs.delete(ipt);
    } else {
      // set not optimized
      this.pinnedInputs.add(ipt);
    }
  };

  private getOnCanvasConstraints = () => {
    const onCanvasConstraints: Num[] = [];
    for (const shape of this.shapes) {
      if (shape.ensureOnCanvas) {
        onCanvasConstraints.push(
          constraints.onCanvas(shape, this.canvas.width, this.canvas.height),
        );
      }
    }
    return onCanvasConstraints;
  };

  private getDragConstraints = () => {
    const getDragConstraint = (shape: Drag & Shape): DragConstraint => {
      if (shape.dragConstraint) {
        return shape.dragConstraint;
      } else {
        return ([x, y]) => [x, y];
      }
    };

    const dragConstraints = new Map<string, DragConstraint>();
    const collect = (shape: Shape) => {
      if (shape.shapeType === ShapeType.Group) {
        shape.shapes.forEach(collect);
      } else if ("drag" in shape && shape.drag) {
        dragConstraints.set(shape.name, getDragConstraint(shape));
      }
    };
    this.shapes.forEach(collect);

    return dragConstraints;
  };

  private getTranslationInputs = () => {
    const inputIdxsByPath: IdxsByPath = new Map();
    const dragInputScales = new Map<string, Map<number, number>>();
    const applyShape = (shape: Shape) => {
      if ("drag" in shape && shape.drag) {
        const scales = new Map<number, number>();
        const axes = new Map<number, number>();
        const mapNum = (num: Num, axis: number): number => {
          const affine = affineCoordinate(num);
          if (
            !affine ||
            affine.coefficients.size !== 1 ||
            !Number.isFinite(affine.offset)
          ) {
            throw new Error(
              `Draggable ${shape.name} coordinates must be affine expressions of one input each; fixed or nonlinear coordinates cannot be dragged.`,
            );
          }
          const [[variable, scale]] = [...affine.coefficients];
          const index = this.varInputMap.get(variable);
          if (index === undefined || !Number.isFinite(scale) || scale === 0) {
            throw new Error(
              `Draggable ${shape.name} coordinates must use inputs from this builder with finite nonzero coefficients.`,
            );
          }
          if (
            (axes.has(index) && axes.get(index) !== axis) ||
            (scales.has(index) && scales.get(index) !== scale)
          ) {
            throw new Error(
              `Draggable ${shape.name} cannot share an input across axes or coordinates with different coefficients.`,
            );
          }
          axes.set(index, axis);
          scales.set(index, scale);
          return index;
        };
        const mapVec2 = (vec: Vec2): any => ({
          tag: "Val",
          contents: { tag: "VectorV", contents: vec.map(mapNum) },
        });
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
        dragInputScales.set(shape.name, scales);
      }

      if (shape.shapeType === ShapeType.Group) {
        shape.shapes.map(applyShape);
      }
    };

    this.shapes.map(applyShape);

    return { inputIdxsByPath, dragInputScales };
  };

  private getNameShapeMap = () => {
    const nameShapeMap = new Map<string, PenroseShape<Num>>();
    const collect = (s: Shape) => {
      nameShapeMap.set(s.name, toPenroseShape(s));
      if (s.shapeType === ShapeType.Group) {
        s.shapes.forEach(collect);
        if (s.clipPath) collect(s.clipPath);
      }
    };
    this.shapes.forEach(collect);
    return nameShapeMap;
  };

  private makeLassoInputs = (): InputInfo[] => {
    return this.inputs.map((i) => ({
      handle: {
        tag: "Var",
        val: i.handle.val,
      },
      meta: {
        init: {
          tag: "Sampled",
          sampler: () => 0,
        },
        stages: "All",
      },
    }));
  };
}

// Module-level active builder context for the JSX runtime.
// _activeBuilder is set when a DiagramBuilder is constructed and within forall callbacks,
// so that the JSX factory can call builder methods without explicit builder reference.
let _activeBuilder: DiagramBuilder | null = null;

/**
 * Get the currently active DiagramBuilder (used by the JSX runtime).
 */
export function getActiveBuilder(): DiagramBuilder | null {
  return _activeBuilder;
}

/**
 * Set the currently active DiagramBuilder (used by the JSX runtime and advanced users).
 */
export function setActiveBuilder(b: DiagramBuilder | null): void {
  _activeBuilder = b;
}

/**
 * Evaluate JSX synchronously in a builder's scope, restoring the previous scope
 * even when the callback throws. Finish JSX construction before awaiting build
 * or render: a module-global context cannot follow an asynchronous callback.
 */
export function withBuilder<T>(
  builder: DiagramBuilder,
  callback: () => T extends PromiseLike<unknown> ? never : T,
): T {
  if (
    ["[object AsyncFunction]", "[object AsyncGeneratorFunction]"].includes(
      Object.prototype.toString.call(callback),
    )
  ) {
    throw new Error(
      "withBuilder callbacks must be synchronous; await after the scope returns.",
    );
  }
  const previous = _activeBuilder;
  _activeBuilder = builder;
  try {
    const result = callback();
    if (
      result !== null &&
      (typeof result === "object" || typeof result === "function") &&
      "then" in result &&
      typeof result.then === "function"
    ) {
      throw new Error(
        "withBuilder callbacks must be synchronous; await after the scope returns.",
      );
    }
    return result;
  } finally {
    _activeBuilder = previous;
  }
}
