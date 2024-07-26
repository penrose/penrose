import {
  Canvas,
  IdxsByPath,
  InputInfo,
  InputMeta,
  Num,
  Shape as PenroseShape,
  Var,
  isVar,
  sampleShape,
  simpleContext,
  uniform,
} from "@penrose/core";
import constraints from "../lib/constraints.js";
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
} from "../types.ts";
import { fromPenroseShape, sortShapes, toPenroseShape } from "../utils.js";
import { Diagram } from "./diagram.js";

export type NamedSamplingContext = {
  makeInput: (meta: InputMeta, name?: string) => Var;
};

export type SelectorVars = Record<string, Type>;
export type SelectorAssignment = Record<string, Substance>;

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

  /**
   * Create a new diagram builder.
   * @param canvas Local dimensions of the SVG. If you find you need thinner lines, try increasing these dimensions.
   * @param variation Randomness seed
   */
  constructor(canvas: Canvas, variation: string) {
    this.canvas = canvas;
    this.variation = variation;

    const { makeInput: createVar } = simpleContext(variation);
    this.samplingContext = {
      makeInput: (meta, name?) => {
        const newVar = createVar(meta);
        this.inputs.push({ handle: newVar, meta });
        if (name !== undefined) {
          if (this.namedInputs.has(name)) {
            throw new Error(`Duplicate vary name ${name}`);
          }
          this.namedInputs.set(name, this.inputs.length - 1);
        }
        this.varInputMap.set(newVar, this.inputs.length - 1);
        return newVar;
      },
    };

    this.defineShapeMethods();

    this.vary({ name: "_time", init: 0, pinned: true });
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

  vary = (opts?: { name?: string; init?: number; pinned?: boolean }): Var => {
    const name = opts?.name;
    const init = opts?.init;
    const pinned = opts?.pinned ?? false;

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

  ensure = (constraint: Num) => {
    this.constraints.push(constraint);
  };

  encourage = (objective: Num) => {
    this.objectives.push(objective);
  };

  layer = (below: Shape, above: Shape) => {
    this.partialLayering.push([below.name, above.name]);
  };

  time = () => {
    return this.getVary("_time");
  }

  getVary = (name: string) => {
    const idx = this.namedInputs.get(name);
    if (idx === undefined) {
      throw new Error(`No input named ${name}`);
    }
    return this.inputs[idx].handle;
  }

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
              tag: "ListV",
              contents: shape.points.map(mapVec2),
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

  build = async (): Promise<Diagram> => {
    this.addDragConstraints();
    this.addOnCanvasConstraints();

    const nameShapeMap = this.getNameShapeMap();
    const penroseShapes = this.shapes.map((s) => toPenroseShape(s));
    const orderedShapes = sortShapes(penroseShapes, this.partialLayering);

    return Diagram.create({
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
    });
  };
}
