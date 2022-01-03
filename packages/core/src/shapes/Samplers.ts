import { Shape } from "types/shape";
import { Path } from "types/style";
import { IColorV, IFloatV, IVectorV, Value } from "types/value";
import { randFloat } from "utils/Util";

/** @ignore */
type PropContents = Value<number>["contents"];
/** @ignore */
type ConstSampler = (type: PropType, value: PropContents) => Sampler;

type Range = [number, number];

// NOTE: I moved `canvasSize` here from Canvas.tsx, which re-exports it, to avoid a circular import in `Style`.

// export const canvasSize: [number, number] = [800, 700];
// export const canvasXRange: Range = [-canvasSize[0] / 2, canvasSize[0] / 2];
// export const canvasYRange: Range = [-canvasSize[1] / 2, canvasSize[1] / 2];
interface ICanvas {
  width: number;
  height: number;
  size: [number, number];
  xRange: Range;
  yRange: Range;
}

export type Canvas = ICanvas;

/** Generate a single string based on a path to a shape */
export const getShapeName = (p: Path): string => {
  if (p.tag === "FieldPath" || p.tag === "PropertyPath") {
    const { name, field } = p;
    return `${name.contents.value}.${field.value}`;
  } else {
    throw new Error("Can only derive shape name from field or property path.");
  }
};

/**
 * Sort shapes given a list of ordered shape names.
 *
 * @param shapes unsorted shapes
 * @param ordering global ordering of shapes
 */
export const sortShapes = (shapes: Shape[], ordering: string[]): Shape[] => {
  return ordering.map(
    (name) =>
      // COMBAK: Deal with nonexistent shapes
      shapes.find(
        ({ properties }) => properties.name.contents === name
      ) as Shape
  ); // assumes that all names are unique
};

/**
 * Checks if an `Equation` shape has non-empty content
 *
 * @param shape a `Equation` shape
 */
export const notEmptyLabel = (shape: Shape): boolean => {
  const { shapeType, properties } = shape;
  return shapeType === "Equation" ? !(properties.string.contents === "") : true;
};

const sampleFloatIn = (min: number, max: number): IFloatV<number> => ({
  tag: "FloatV",
  contents: randFloat(min, max),
});

const vectorSampler: Sampler = (canvas): IVectorV<number> => ({
  tag: "VectorV",
  contents: [randFloat(...canvas.xRange), randFloat(...canvas.yRange)],
});
const widthSampler: Sampler = (canvas): IFloatV<number> => ({
  tag: "FloatV",
  contents: randFloat(3, canvas.width / 6),
});
const zeroFloat: Sampler = (_canvas): IFloatV<number> => ({
  tag: "FloatV",
  contents: 0.0,
});
const pathLengthSampler: Sampler = (_canvas): IFloatV<number> => ({
  tag: "FloatV",
  contents: 1.0,
});
const heightSampler: Sampler = (canvas): IFloatV<number> => ({
  tag: "FloatV",
  contents: randFloat(3, canvas.height / 6),
});
const strokeSampler: Sampler = (_canvas): IFloatV<number> => ({
  tag: "FloatV",
  contents: randFloat(0.5, 3),
});
const colorSampler: Sampler = (_canvas): IColorV<number> => {
  const [min, max] = [0.1, 0.9];
  return {
    tag: "ColorV",
    contents: {
      tag: "RGBA",
      contents: [
        randFloat(min, max),
        randFloat(min, max),
        randFloat(min, max),
        0.5,
      ],
    },
  };
};

export const constValue: ConstSampler = (
  tag: PropType,
  contents: PropContents
) => (_canvas) =>
  ({
    tag,
    contents,
  } as Value<number>);

const black: IColorV<number> = {
  tag: "ColorV",
  contents: {
    tag: "RGBA",
    contents: [0, 0, 0, 1.0],
  },
};

const noPaint: IColorV<number> = {
  tag: "ColorV",
  contents: {
    tag: "NONE",
  },
};

type PropType = Value<number>["tag"];

type Sampler = (canvas: Canvas) => Value<number>;

// note: given e.g. the difference in fillOpacity between Polygon and Polyline,
// it may be a bad idea for a property to always have the same sampler based on
// its name
export const samplers = {
  // INamed
  name: constValue("StrV", "defaultCircle"),

  // IStroke
  strokeWidth: strokeSampler,
  strokeStyle: constValue("StrV", "solid"),
  strokeColor: (): Value<number> => noPaint, // or maybe colorSampler
  strokeDashArray: constValue("StrV", ""),

  // IFill
  fillColor: (): Value<number> => noPaint, // or maybe colorSampler

  // ICenter
  center: vectorSampler,

  // IRect
  width: widthSampler,
  height: heightSampler,

  // IArrow
  arrowheadSize: constValue("FloatV", 1.0),
  arrowheadStyle: constValue("StrV", "arrowhead-2"),
  startArrowhead: constValue("BoolV", false),
  endArrowhead: constValue("BoolV", false),

  // ICorner
  cornerRadius: zeroFloat,

  // IRotate
  rotation: constValue("FloatV", 0),
  // IScale
  scale: constValue("FloatV", 1),

  // IPoly
  points: constValue("PtListV", [
    [0, 0],
    [0, 10],
    [10, 0],
  ]),

  // IString
  string: constValue("StrV", "Text"),

  // ICircle
  r: widthSampler,

  // IEllipse
  rx: widthSampler,
  ry: heightSampler,

  // IImage
  href: constValue("StrV", "missing image path"),

  // IText
  visibility: constValue("StrV", ""),
  fontFamily: constValue("StrV", ""),
  fontSize: constValue("StrV", "12pt"),
  fontSizeAdjust: constValue("StrV", ""),
  fontStretch: constValue("StrV", ""),
  fontStyle: constValue("StrV", ""),
  fontVariant: constValue("StrV", ""),
  fontWeight: constValue("StrV", ""),
  textAnchor: constValue("StrV", "middle"),
  alignmentBaseline: constValue("StrV", "middle"),
};
