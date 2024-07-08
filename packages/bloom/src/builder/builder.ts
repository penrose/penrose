import {
  Canvas,
  InputMeta,
  Num,
  Shape as PenroseShape,
  Var,
  constrDict,
  sampleShape,
  simpleContext,
  uniform,
} from "@penrose/core";
import { Diagram } from "../renderer/Renderer.js";
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
  Rectangle,
  RectangleProps,
  Shape,
  ShapeProps,
  ShapeType,
  TextProps,
} from "./types.js";
import { fromPenroseShape, sortShapes, toPenroseShape } from "./utils.js";

export type NamedSamplingContext = {
  makeInput: (meta: InputMeta, name?: string) => Var;
};

export class DiagramBuilder {
  private canvas: Canvas;
  private inputs: Var[] = [];
  private inputInits: ("Pending" | "Sampled")[] = [];
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
        this.inputs.push(newVar);
        this.inputInits.push(meta.init.tag);
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
          if (penroseShape.ensureOnCanvas) {
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

  vary = (name?: string): Var => {
    const newVar = this.samplingContext.makeInput(
      {
        init: { tag: "Sampled", sampler: uniform(...this.canvas.xRange) },
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
    console.log(orderedShapes.map((s) => s.name.contents));

    return Diagram.create({
      canvas: this.canvas,
      variation: this.variation,
      inputs: this.inputs,
      inputInits: this.inputInits,
      constraints: this.constraints,
      objectives: this.objectives,
      shapes: orderedShapes,
      // namedInputs: this.namedInputs,
    });
  };
}
