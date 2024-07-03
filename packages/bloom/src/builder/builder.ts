import {
  boolV,
  Canvas,
  floatV,
  InputMeta,
  Num,
  sampleShape,
  Shape as PenroseShape,
  simpleContext,
  strV,
  uniform,
  Value,
  Var, clipDataV, colorV, ptListV, vectorV,
  Group as PenroseGroup, constrDict
} from "@penrose/core";
import _ from "lodash";
import { Diagram } from "../renderer/Renderer.tsx";
import {
  Circle,
  CircleProps,
  clip,
  Clip,
  Color, Ellipse, EllipseProps, Equation, EquationProps, Group, GroupProps, Image, ImageProps, Line, LineProps,
  noClip,
  NoClip, PathProps,
  PenroseShapeType, Polygon, PolygonProps, Polyline, PolylineProps, Rectangle, RectangleProps,
  Shape,
  ShapeProps,
  ShapeType, TextProps, Path
} from "./types.js";

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
      Object.defineProperty(
        this, firstLetterLower(shapeType), {
          value: (props: Partial<ShapeProps> = {}): Shape => {
            if (!("name" in props)) {
              props.name = String(this.nextId++) + "_" + shapeType;
            }
            const penroseShape = this.toPenroseShape(shapeType, props);
            this.shapes.push(penroseShape);
            if (penroseShape.ensureOnCanvas) {
              this.ensure(constrDict.onCanvas.body(penroseShape, this.canvas.width, this.canvas.height).value)
            }
            return this.fromPenroseShape(penroseShape) as Shape;
          },
        }
      );
    }
    console.log(this);
  }

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

  private toPenroseShape = (
    shapeType: ShapeType,
    props: Partial<ShapeProps>
  ): PenroseShape<Num> => {
    const penroseShapeProps = sampleShape(
      shapeType,
      this.samplingContext,
      this.canvas,
    );

    for (const [prop, value] of Object.entries(props)) {
      let resultV: Value.Value<Num>;
      switch (PenroseShapeType.get(shapeType)![prop]) {
        case "FloatV":
          resultV = floatV(value);
          break;

        case "StrV":
          resultV = strV(value);
          break;

        case "VectorV":
          resultV = vectorV(value);
          break;

        case "ClipDataV":
          switch ((value as NoClip | Clip).tag) {
            case "NoClip":
              resultV = clipDataV(value);
              break;

            case "Clip":
              resultV = clipDataV({
                tag: "Clip",
                contents: this.toPenroseShape(
                  (value as Clip).shape.shapeType,
                  (value as Clip).shape
                ) as Exclude<PenroseShape<Num>, PenroseGroup<Num>>,
              })
          }
          break;

        case "PtListV":
          resultV = ptListV(value);
          break;

        case "ColorV":
          resultV = colorV({
            tag: "RGBA",
            contents: value as Color
          });
          break;

        case "BoolV":
          resultV = boolV(value);
          break;

        default:
          throw new Error(`Unknown field type for ${prop}`);
      }

      _.set(penroseShapeProps, prop, resultV);
    }

    return {
      ...penroseShapeProps,
      shapeType: shapeType,
      passthrough: new Map(),
    } as PenroseShape<Num>;
  };

  private fromPenroseShape =(
    penroseShape: PenroseShape<Num>
  ): Shape => {
    const shape: Partial<Shape> = {};
    const shapeTypes = PenroseShapeType.get(ShapeType[penroseShape.shapeType])!;

    for (const [prop, value] of Object.entries(penroseShape)) {
      switch (shapeTypes[prop]) {
        case "FloatV":
        case "StrV":
        case "VectorV":
        case "PtListV":
        case "BoolV":
          _.set(shape, prop, value.contents);
          break;

        case "ClipDataV":
          switch (value.tag) {
            case "NoClip":
              _.set(shape, prop, noClip());
              break;

            case "Clip":
              _.set(shape, prop, clip(
                this.fromPenroseShape(value.shape) as Exclude<Shape, Group>
              ));
              break;
          }
          break;

        case "ColorV":
          _.set(shape, prop, value.contents.contents);
          break;
      }
    }

    return shape as unknown as Shape;
  }

  // for typing only; dynamically generated

  circle = (props: Partial<CircleProps> = {}): Readonly<Circle> => { throw new Error("Not filled"); }
  ellipse = (props: Partial<EllipseProps> = {}): Readonly<Ellipse> => { throw new Error("Not filled"); }
  equation = (props: Partial<EquationProps> = {}): Readonly<Equation> => { throw new Error("Not filled"); }
  group = (props: Partial<GroupProps> = {}): Readonly<Group> => { throw new Error("Not filled"); }
  image = (props: Partial<ImageProps> = {}): Readonly<Image> => { throw new Error("Not filled"); }
  line = (props: Partial<LineProps> = {}): Readonly<Line> => { throw new Error("Not filled"); }
  path = (props: Partial<PathProps> = {}): Readonly<Path> => { throw new Error("Not filled"); }
  polygon = (props: Partial<PolygonProps> = {}): Readonly<Polygon> => { throw new Error("Not filled"); }
  polyline = (props: Partial<PolylineProps> = {}): Readonly<Polyline> => { throw new Error("Not filled"); }
  rectangle = (props: Partial<RectangleProps> = {}): Readonly<Rectangle> => { throw new Error("Not filled"); }
  text = (props: Partial<TextProps> = {}): Readonly<Text> => { throw new Error("Not filled"); }

  ensure = (constraint: Num) => {
    this.constraints.push(constraint);
  };

  encourage = (objective: Num) => {
    this.objectives.push(objective);
  };

  build = async (): Promise<Diagram> => {
    return Diagram.create({
      canvas: this.canvas,
      variation: this.variation,
      inputs: this.inputs,
      inputInits: this.inputInits,
      constraints: this.constraints,
      objectives: this.objectives,
      shapes: this.shapes,
      namedInputs: this.namedInputs,
    });
  };
}
