import {
  boolV,
  Canvas, clipDataV, colorV, floatV, Group as PenroseGroup,
  LabelCache, Num, pathDataV,
  PathResolver, ptListV,
  RenderShapes, Shape as PenroseShape,
  strV, Value, vectorV
} from "@penrose/core";
import _ from "lodash";
import { Clip, Color, NoClip, PenroseShapeType, Shape } from "./types.js";
export const stateToSVG = async (
  state: {
    canvas: Canvas;
    shapes: Shape<number>[];
    labelCache: LabelCache;
    variation: string;
  },
  config: {
    pathResolver: PathResolver;
    width: string;
    height: string;
    texLabels: boolean;
  },
): Promise<SVGSVGElement> => {
  const { canvas, shapes, labelCache, variation } = state;
  // render the current frame
  const rendered = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "svg",
  );
  rendered.setAttribute("version", "1.2");
  rendered.setAttribute("xmlns", "http://www.w3.org/2000/svg");
  rendered.setAttribute("viewBox", `0 0 ${canvas.width} ${canvas.height}`);
  await RenderShapes(shapes, rendered, {
    labels: labelCache,
    canvasSize: canvas.size,
    variation,
    namespace: "editor",
    texLabels: config.texLabels,
    pathResolver: config.pathResolver,
  });
  rendered.setAttribute("width", config.width);
  rendered.setAttribute("height", config.height);
  return rendered;
};

export const toPenroseShape = (
  shape: Partial<Shape> & Required<Pick<Shape, "shapeType">>,
  base?: Partial<PenroseShape<Num>>
): PenroseShape<Num> => {
  const penroseShape: Partial<PenroseShape<Num>> = base ?? {};

  for (const [prop, value] of Object.entries(shape)) {
    if (prop === "shapeType") continue;

    let resultV: Value.Value<Num>;
    switch (PenroseShapeType.get(shape.shapeType)![prop]) {
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
              contents: this.sampleAndFillPenroseShape(
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

      case "PathDataV":
        resultV = pathDataV(value);
        break;

      default:
        throw new Error(`Unknown field type for ${prop}`);
    }

    _.set(penroseShape, prop, resultV);
  }

  return {
    ...penroseShape,
    shapeType: shape.shapeType,
    passthrough: new Map(),
  } as PenroseShape<Num>;
}
