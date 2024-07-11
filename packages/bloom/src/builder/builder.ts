import {
  Canvas,
  InputInfo,
  InputMeta,
  Num,
  Shape as PenroseShape,
  Var,
  constrDict,
  sampleShape,
  simpleContext,
  uniform,
} from "@penrose/core";
import {
  Circle,
  CircleProps,
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
} from "../types.ts";
import { fromPenroseShape, sortShapes, toPenroseShape } from "../utils.js";
import { Diagram } from "./diagram.js";

export type NamedSamplingContext = {
  makeInput: (meta: InputMeta, name?: string) => Var;
};

export type SelectorVars = Record<string, Type>;
export type SelectorAssignment = Record<string, Substance>;

export class DiagramBuilder {
  /** Substance/Domain stuff */
  private substanceTypeMap: Map<Substance, Type> = new Map();

  private canvas: Canvas;
  private inputs: InputInfo[] = [];
  private namedInputs: Map<string, number> = new Map();
  private samplingContext: NamedSamplingContext;
  private shapes: PenroseShape<Num>[] = [];
  private constraints: Num[] = [];
  private objectives: Num[] = [];
  private variation: string;
  private nextId = 0;
  private partialLayering: [string, string][] = [];

  constructor(canvas: Canvas, variation: string) {
    this.canvas = canvas;
    this.variation = variation;

    const { makeInput: createVar } = simpleContext(variation);
    this.samplingContext = {
      makeInput: (meta, name?) => {
        const newVar = createVar(meta);
        this.inputs.push({ handle: newVar, meta });
        if (name !== undefined) {
          this.namedInputs.set(name, this.inputs.length - 1);
        }
        return newVar;
      },
    };

    this.defineShapeMethods();
  }

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
          const penroseShape = this.sampleAndFillPenroseShape(shapeType, props);
          this.shapes.push(penroseShape);
          if (penroseShape.ensureOnCanvas.contents) {
            this.ensure(
              constrDict.onCanvas.body(
                penroseShape,
                this.canvas.width,
                this.canvas.height,
              ).value,
            );
          }
          return fromPenroseShape(penroseShape);
        },
      });
    }
  };

  private substance = (type: Type): Substance => {
    const newSubstance: Substance = {};
    this.substanceTypeMap.set(newSubstance, type);
    return newSubstance;
  };

  type = (): Type => {
    const substance = this.substance.bind(this);
    return function t() {
      return substance(t);
    };
  };

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

  forall = (
    vars: SelectorVars,
    func: (assigned: SelectorAssignment, matchId: number) => void,
  ) => {
    const varTypePairs: { name: string; type: Type }[] = Object.entries(
      vars,
    ).map(([v, t]) => ({
      name: v,
      type: t,
    }));
    const substances = [...this.substanceTypeMap.keys()];

    const traverseSubstances = (
      prevPairs: { name: string; subst: Substance }[],
      nextPairs: { name: string; type: Type }[],
      startI: number,
      matches: number,
    ): number => {
      if (nextPairs.length === 0) {
        const assignment: SelectorAssignment = {};
        for (const { name, subst } of prevPairs) {
          assignment[name] = subst;
        }
        func(assignment, matches);
        return matches;
      }

      for (let i = startI; i < substances.length; ++i) {
        const subst = substances[i];
        const type = this.substanceTypeMap.get(subst)!;
        if (nextPairs[0].type === type) {
          matches += traverseSubstances(
            prevPairs.concat([{ name: nextPairs[0].name, subst }]),
            nextPairs.slice(1),
            i + 1,
            matches,
          );
        }
      }

      return matches;
    };

    traverseSubstances([], varTypePairs, 0, 0);
  };

  vary = (name?: string, init?: number): Var => {
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
    return newVar;
  };

  private sampleAndFillPenroseShape = (
    shapeType: ShapeType,
    props: Partial<ShapeProps>,
  ): PenroseShape<Num> => {
    const penroseShapeBase = sampleShape(
      shapeType,
      this.samplingContext,
      this.canvas,
    );

    return toPenroseShape(
      {
        ...props,
        shapeType,
      },
      penroseShapeBase,
    );
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

  build = async (): Promise<Diagram> => {
    const orderedShapes = sortShapes(this.shapes, this.partialLayering);
    return Diagram.create({
      canvas: this.canvas,
      variation: this.variation,
      inputs: this.inputs,
      constraints: this.constraints,
      objectives: this.objectives,
      shapes: orderedShapes,
      namedInputs: this.namedInputs,
    });
  };
}
