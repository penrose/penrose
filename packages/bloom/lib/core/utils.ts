import {
  Graph,
  Num,
  PathResolver,
  Shape as PenroseShape,
  PenroseState,
  RenderShapes,
  Result,
  Value,
  boolV,
  colorV,
  err,
  floatV,
  ok,
  pathDataV,
  ptListV,
  strV,
  vectorV,
} from "@penrose/core";
import _ from "lodash";
import { browserAdaptor } from "mathjax-full/js/adaptors/browserAdaptor";
import { HTMLHandler } from "mathjax-full/js/handlers/html/HTMLHandler";
import { TeX } from "mathjax-full/js/input/tex";
import { AllPackages } from "mathjax-full/js/input/tex/AllPackages";
import { mathjax } from "mathjax-full/js/mathjax";
import { SVG } from "mathjax-full/js/output/svg";
import {
  Color,
  Shape,
  ShapeProps,
  ShapeType,
  penroseShapeFieldTypes,
} from "./types.js";

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
  const fieldTypes = penroseShapeFieldTypes.get(shape.shapeType)!;

  for (const [prop, value] of Object.entries(shape)) {
    if (prop === "shapeType" || fieldTypes[prop] === undefined) continue;

    let resultV: Value.Value<Num>;
    switch (penroseShapeFieldTypes.get(shape.shapeType)![prop]) {
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
            penroseShapeFieldTypes.get(shape.shapeType)![prop]
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

export const fromPenroseShape = (
  penroseShape: PenroseShape<Num>,
  base?: Partial<ShapeProps>,
): Shape => {
  if (penroseShape.shapeType === "Group") {
    throw new Error("Groups not yet supported in bloom");
  }

  const shape: Partial<Shape> = {
    shapeType: ShapeType[penroseShape.shapeType],
    ...(base ?? {}),
  };
  const shapeTypes = penroseShapeFieldTypes.get(
    ShapeType[penroseShape.shapeType],
  )!;

  for (const [prop, value] of Object.entries(penroseShape)) {
    if (prop in shape) continue;
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

export const stateToSVG = async (
  state: PenroseState,
  config: {
    pathResolver: PathResolver;
    texLabels: boolean;
    titleCache?: Map<string, SVGElement>;
  },
): Promise<SVGSVGElement> => {
  const { canvas, labelCache, variation, computeShapes, varyingValues } = state;
  const shapes = computeShapes(varyingValues);

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
    namespace: "",
    texLabels: config.texLabels,
    pathResolver: config.pathResolver,
    titleCache: config.titleCache,
  });
  return rendered;
};

export const mathjaxInitWithHandler = () => {
  const adaptor = browserAdaptor();
  const handler = new HTMLHandler(adaptor);
  mathjax.handlers.add(handler, 1);

  const tex = new TeX({
    packages: AllPackages,
    macros: {
      textsc: ["\\style{font-variant-caps: small-caps}{\\text{#1}}", 1],
    },
    inlineMath: [
      ["$", "$"],
      ["\\(", "\\)"],
    ],
    processEscapes: true,
    // https://github.com/mathjax/MathJax-demos-node/issues/25#issuecomment-711247252
    formatError: (jax: unknown, err: Error) => {
      throw Error(err.message);
    },
  });
  const svg = new SVG({ fontCache: "none" });
  const html = mathjax.document("", { InputJax: tex, OutputJax: svg });

  const convert = (input: string): Result<HTMLElement, string> => {
    // HACK: workaround for newlines. This workaround will force MathJax to always return the same heights regardless of the text content.
    // https://github.com/mathjax/MathJax/issues/2312#issuecomment-538185951
    // if(input) {
    //   const newline_escaped = `\\displaylines{${input}}`;
    // }
    try {
      const node = html.convert(input, {});
      return ok(node.firstChild);
    } catch (error: any) {
      return err(error.message);
    }
  };

  return { convert, handler };
};

export { makeCanvas as canvas } from "@penrose/core";
