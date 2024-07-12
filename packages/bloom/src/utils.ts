import {
  Canvas,
  Graph,
  LabelCache,
  Num,
  PathResolver,
  Shape as PenroseShape,
  RenderShapes,
  Value,
  boolV,
  colorV,
  floatV,
  pathDataV,
  ptListV,
  strV,
  vectorV,
} from "@penrose/core";
import _ from "lodash";
import { Color, PenroseShapeType, Shape, ShapeType } from "./types.js";
export const stateToSVG = async (
  state: {
    canvas: Canvas;
    shapes: PenroseShape<number>[];
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

export const fromPenroseColor = (colorV: Value.ColorV<Num>): Color => {
  if (colorV.contents.tag === "NONE") {
    return [0, 0, 0, 0];
  } else {
    return colorV.contents.contents;
  }
};

export const toPenroseColor = (color: Color): Value.Color<Num> => {
  return {
    tag: "RGBA",
    contents: color,
  };
};

export const toPenroseShape = (
  shape: Partial<Shape> & Required<Pick<Shape, "shapeType">>,
  base?: Partial<PenroseShape<Num>>,
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

      case "PtListV":
        resultV = ptListV(value);
        break;

      case "ColorV":
        resultV = colorV(toPenroseColor(value));
        break;

      case "BoolV":
        resultV = boolV(value);
        break;

      case "PathDataV":
        resultV = pathDataV(value);
        break;

      default:
        throw new Error(
          `Unknown field type ${
            PenroseShapeType.get(shape.shapeType)![prop]
          } for ${prop}`,
        );
    }

    _.set(penroseShape, prop, resultV);
  }

  return {
    ...penroseShape,
    shapeType: shape.shapeType,
    passthrough: new Map(),
  } as PenroseShape<Num>;
};

export const fromPenroseShape = (penroseShape: PenroseShape<Num>): Shape => {
  if (penroseShape.shapeType === "Group") {
    throw new Error("Groups not yet supported in bloom");
  }

  const shape: Partial<Shape> = {
    shapeType: ShapeType[penroseShape.shapeType],
  };
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

      case "ColorV":
        _.set(shape, prop, fromPenroseColor(value));
        break;

      // default, nothing
    }
  }

  return shape as unknown as Shape;
};

export const sortShapes = (
  shapes: PenroseShape<Num>[],
  partialLayering: [string, string][],
): PenroseShape<Num>[] => {
  const layerGraph = new Graph<string>();
  for (const { name } of shapes) {
    layerGraph.setNode(name.contents, undefined);
  }
  for (const [below, above] of partialLayering) {
    layerGraph.setEdge({ i: below, j: above, e: undefined });
  }
  const sortedNames = layerGraph.topsort();
  const nameIndices = new Map(sortedNames.map((name, i) => [name, i]));
  return shapes.sort(
    (a, b) =>
      nameIndices.get(a.name.contents)! - nameIndices.get(b.name.contents)!,
  );
};

export class CallbackLooper {
  private messageChannel?: MessageChannel;
  private messageChannelPromise?: Promise<void> | null;
  private messageChannelResolve?: () => void | null;

  private activeId = 0;
  private running = false;
  private queuer: (callback: () => unknown) => Promise<void>;

  constructor(loopType: "MessageChannel" | "AnimationFrame") {
    switch (loopType) {
      case "MessageChannel":
        this.messageChannel = new MessageChannel();
        this.messageChannel!.port1.onmessage = () => {
          this.messageChannelResolve!();
          this.setPromiseAndResolve();
        };
        this.setPromiseAndResolve();
        this.queuer = async (callback) => {
          this.messageChannel!.port2.postMessage(null);
          await this.messageChannelPromise!;
          this.setPromiseAndResolve();
          callback();
        };
        break;

      case "AnimationFrame":
        this.queuer = async (callback) => {
          requestAnimationFrame(callback);
        };
        break;
    }
  }

  setPromiseAndResolve = () => {
    this.messageChannelPromise = new Promise((resolve) => {
      this.messageChannelResolve = resolve;
    });
  };

  loop = (callback: () => Promise<boolean>) => {
    this.running = true;
    const startingId = ++this.activeId;

    const onFrame = async () => {
      if (this.activeId === startingId) {
        if (await callback()) {
          this.queuer(onFrame);
        } else {
          this.running = false;
        }
      } // otherwise, we were interrupted by another loop call, so someone is
      // running, hence don't turn `this.running` off
    };
    this.queuer(onFrame);
  };

  isRunning = () => this.running;

  stop = () => this.loop(async () => false);
}

export { makeCanvas as canvas } from "@penrose/core";
