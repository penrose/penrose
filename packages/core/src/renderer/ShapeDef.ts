import { absVal, constOf, max, ops, sub, mul, min } from "engine/Autodiff";
import * as BBox from "engine/BBox";
import { randFloat } from "utils/Util";
import { Properties, Shape } from "types/shape";
import { Value } from "types/value";
import { IFloatV, IVectorV, IColorV } from "types/value";
import { Path } from "types/style";
import { isPt2, Pt2, VarAD } from "types/ad";

//#region shapedef helpers and samplers

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
 * Checks if a `Text` shape has non-empty content
 *
 * @param shape a `Text` shape
 */
export const notEmptyLabel = (shape: Shape): boolean => {
  const { shapeType, properties } = shape;
  return shapeType === "Text" ? !(properties.string.contents === "") : true;
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

//#endregion

//#region shapedefs
export type ShapeDef = IShapeDef;

// type HasTag<T, N> = T extends { tag: N } ? T : never;

export type PropType = Value<number>["tag"];
export type IPropModel = { [k: string]: [PropType, Sampler] };

export interface IShapeDef {
  shapeType: string;
  properties: IPropModel;
  positionalProps?: string[];
  bbox: (s: Properties<VarAD>) => BBox.BBox;
}

export type Sampler = (canvas: Canvas) => Value<number>;

const bboxFromCircle = ({ r, center }: Properties<VarAD>): BBox.BBox => {
  // https://github.com/penrose/penrose/issues/701
  if (r.tag !== "FloatV") {
    throw new Error(`bboxFromCircle expected r to be FloatV, but got ${r.tag}`);
  }
  if (center.tag !== "VectorV") {
    throw new Error(
      `bboxFromCircle expected center to be VectorV, but got ${center.tag}`
    );
  }
  if (!isPt2(center.contents)) {
    throw new Error(
      `bboxFromCircle expected center to be Pt2, but got length ${center.contents.length}`
    );
  }

  const diameter = mul(constOf(2), r.contents);
  // TODO: figure out whether this is OK or if I should clone diameter
  return BBox.bbox(diameter, diameter, center.contents);
};

export const circleDef: ShapeDef = {
  shapeType: "Circle",
  properties: {
    center: ["VectorV", vectorSampler],
    r: ["FloatV", widthSampler],
    pathLength: ["FloatV", pathLengthSampler], // part of svg spec
    strokeWidth: ["FloatV", strokeSampler],
    style: ["StrV", constValue("StrV", "filled")],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", () => noPaint],
    color: ["ColorV", colorSampler],
    name: ["StrV", constValue("StrV", "defaultCircle")],
  },
  positionalProps: ["center"],
  bbox: bboxFromCircle,
};

const bboxFromEllipse = ({ rx, ry, center }: Properties<VarAD>): BBox.BBox => {
  // https://github.com/penrose/penrose/issues/701
  if (rx.tag !== "FloatV") {
    throw new Error(
      `bboxFromEllipse expected rx to be FloatV, but got ${rx.tag}`
    );
  }
  if (ry.tag !== "FloatV") {
    throw new Error(
      `bboxFromEllipse expected ry to be FloatV, but got ${ry.tag}`
    );
  }
  if (center.tag !== "VectorV") {
    throw new Error(
      `bboxFromEllipse expected center to be VectorV, but got ${center.tag}`
    );
  }
  if (!isPt2(center.contents)) {
    throw new Error(
      `bboxFromEllipse expected center to be Pt2, but got length ${center.contents.length}`
    );
  }

  const dx = mul(constOf(2), rx.contents);
  const dy = mul(constOf(2), ry.contents);
  return BBox.bbox(dx, dy, center.contents);
};

export const ellipseDef: ShapeDef = {
  shapeType: "Ellipse",
  properties: {
    center: ["VectorV", vectorSampler],
    rx: ["FloatV", widthSampler],
    ry: ["FloatV", heightSampler],
    pathLength: ["FloatV", pathLengthSampler], // part of svg spec
    strokeWidth: ["FloatV", strokeSampler],
    style: ["StrV", constValue("StrV", "filled")],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", () => noPaint],
    strokeDashArray: ["StrV", constValue("StrV", "")],
    color: ["ColorV", colorSampler],
    name: ["StrV", constValue("StrV", "defaultCircle")],
  },
  positionalProps: ["center"],
  bbox: bboxFromEllipse,
};

const bboxFromRect = ({ w, h, center }: Properties<VarAD>): BBox.BBox => {
  // https://github.com/penrose/penrose/issues/701
  if (w.tag !== "FloatV") {
    throw new Error(`bboxFromRect expected w to be FloatV, but got ${w.tag}`);
  }
  if (h.tag !== "FloatV") {
    throw new Error(`bboxFromRect expected h to be FloatV, but got ${h.tag}`);
  }
  if (center.tag !== "VectorV") {
    throw new Error(
      `bboxFromRect expected center to be VectorV, but got ${center.tag}`
    );
  }
  if (!isPt2(center.contents)) {
    throw new Error(
      `bboxFromRect expected center to be Pt2, but got length ${center.contents.length}`
    );
  }

  // TODO: figure out whether I need to handle rx
  return BBox.bbox(w.contents, h.contents, center.contents);
};

export const rectDef: ShapeDef = {
  shapeType: "Rectangle",
  properties: {
    center: ["VectorV", vectorSampler],
    w: ["FloatV", widthSampler],
    h: ["FloatV", heightSampler],
    rx: ["FloatV", zeroFloat],
    strokeWidth: ["FloatV", strokeSampler],
    style: ["StrV", constValue("StrV", "filled")],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", () => noPaint],
    strokeDashArray: ["StrV", constValue("StrV", "")],
    color: ["ColorV", colorSampler],
    name: ["StrV", constValue("StrV", "defaultRect")],
  },
  positionalProps: ["center"],
  bbox: bboxFromRect,
};

const bboxFromCallout = (_: Properties<VarAD>): BBox.BBox => {
  throw new Error("bboxFromCallout not implemented"); // TODO
};

export const calloutDef: ShapeDef = {
  shapeType: "Callout",
  properties: {
    anchor: ["VectorV", vectorSampler],
    center: ["VectorV", vectorSampler],
    w: ["FloatV", widthSampler],
    h: ["FloatV", heightSampler],
    padding: ["FloatV", zeroFloat], // padding around the contents of the callout box
    rx: ["FloatV", zeroFloat], // currently unused
    strokeWidth: ["FloatV", strokeSampler],
    style: ["StrV", constValue("StrV", "filled")],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", () => noPaint],
    strokeDashArray: ["StrV", constValue("StrV", "")],
    color: ["ColorV", colorSampler],
    name: ["StrV", constValue("StrV", "defaultCallout")],
  },
  bbox: bboxFromCallout,
};

const bboxFromPolygon = (s: Properties<VarAD>): BBox.BBox => {
  // TODO: handle scale
  // https://github.com/penrose/penrose/issues/709
  throw new Error("bboxFromPolygon not implemented"); // TODO
};

export const polygonDef: ShapeDef = {
  shapeType: "Polygon",
  properties: {
    strokeWidth: ["FloatV", strokeSampler],
    style: ["StrV", constValue("StrV", "filled")],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", () => noPaint],
    color: ["ColorV", colorSampler],
    center: ["VectorV", vectorSampler],
    scale: ["FloatV", constValue("FloatV", 1)],
    name: ["StrV", constValue("StrV", "defaultPolygon")],
    points: [
      "PtListV",
      constValue("PtListV", [
        [0, 0],
        [0, 10],
        [10, 0],
      ]),
    ],
  },
  positionalProps: ["center"],
  bbox: bboxFromPolygon,
};

const bboxFromFreeformPolygon = ({ points }: Properties<VarAD>): BBox.BBox => {
  // https://github.com/penrose/penrose/issues/701
  // TODO: figure out why this isn't PtListV
  if (points.tag !== "LListV") {
    throw new Error(
      `bboxFromFreeformPolygon expected points to be LListV, but got ${points.tag}`
    );
  }

  // TODO: figure out whether I need to handle scale

  const pts: Pt2[] = points.contents.map((point) => {
    if (isPt2(point)) {
      return point;
    } else {
      throw new Error(
        `bboxFromFreeformPolygon expected each point to be Pt2, but got length ${point.length}`
      );
    }
  });
  const minCorner = pts.reduce((corner: Pt2, point: Pt2) => [
    min(corner[0], point[0]),
    min(corner[1], point[1]),
  ]);
  const maxCorner = pts.reduce((corner: Pt2, point: Pt2) => [
    max(corner[0], point[0]),
    max(corner[1], point[1]),
  ]);
  const w = sub(maxCorner[0], minCorner[0]);
  const h = sub(maxCorner[1], minCorner[1]);
  const center = ops.vdiv(ops.vadd(minCorner, maxCorner), constOf(2));
  if (!isPt2(center)) {
    throw new Error("ops.vadd and ops.vdiv did not preserve dimension");
  }
  return BBox.bbox(w, h, center);
};

export const freeformPolygonDef: ShapeDef = {
  shapeType: "FreeformPolygon",
  properties: {
    strokeWidth: ["FloatV", strokeSampler],
    style: ["StrV", constValue("StrV", "filled")],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", () => noPaint],
    color: ["ColorV", colorSampler],
    name: ["StrV", constValue("StrV", "defaultFreeformPolygon")],
    scale: ["FloatV", constValue("FloatV", 1)],
    points: [
      "PtListV",
      constValue("PtListV", [
        [0, 0],
        [0, 10],
        [10, 0],
      ]),
    ],
  },
  positionalProps: [],
  bbox: bboxFromFreeformPolygon,
};

const DEFAULT_PATHSTR = `M 10,30
A 20,20 0,0,1 50,30
A 20,20 0,0,1 90,30
Q 90,60 50,90
Q 10,60 10,30 z`;

const bboxFromPathString = (_: Properties<VarAD>): BBox.BBox => {
  throw new Error("bboxFromPathString not implemented"); // TODO
};

export const pathStringDef: ShapeDef = {
  shapeType: "PathString",
  properties: {
    center: ["VectorV", vectorSampler],
    w: ["FloatV", widthSampler],
    h: ["FloatV", heightSampler],
    rotation: ["FloatV", constValue("FloatV", 0)],
    opacity: ["FloatV", constValue("FloatV", 1.0)],
    strokeWidth: ["FloatV", strokeSampler],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", colorSampler],
    color: ["ColorV", () => noPaint],
    name: ["StrV", constValue("StrV", "defaultPolygon")],
    data: ["StrV", constValue("StrV", DEFAULT_PATHSTR)],
    viewBox: ["StrV", constValue("StrV", "0 0 100 100")],
  },
  positionalProps: ["center"],
  bbox: bboxFromPathString,
};

const bboxFromPolyline = (_: Properties<VarAD>): BBox.BBox => {
  throw new Error("bboxFromPolyline not implemented"); // TODO
};

export const polylineDef: ShapeDef = {
  shapeType: "Polyline",
  properties: {
    strokeWidth: ["FloatV", strokeSampler],
    center: ["VectorV", vectorSampler],
    scale: ["FloatV", constValue("FloatV", 1)],
    style: ["StrV", constValue("StrV", "filled")],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", colorSampler],
    color: ["ColorV", colorSampler],
    name: ["StrV", constValue("StrV", "defaultPolygon")],
    points: [
      "PtListV",
      constValue("PtListV", [
        [0, 0],
        [0, 10],
        [10, 0],
      ]),
    ],
  },
  positionalProps: ["center"],
  bbox: bboxFromPolyline,
};

export const imageDef: ShapeDef = {
  shapeType: "Image",
  properties: {
    center: ["VectorV", vectorSampler],
    w: ["FloatV", widthSampler],
    h: ["FloatV", heightSampler],
    rotation: ["FloatV", constValue("FloatV", 0)],
    opacity: ["FloatV", constValue("FloatV", 1.0)],
    style: ["StrV", constValue("StrV", "filled")],
    stroke: ["StrV", constValue("StrV", "none")],
    path: ["StrV", constValue("StrV", "missing image path")],
    name: ["StrV", constValue("StrV", "defaultImage")],
  },
  positionalProps: ["center"],
  bbox: bboxFromRect, // TODO: handle rotation
};

const bboxFromSquare = ({ side, center }: Properties<VarAD>): BBox.BBox => {
  // https://github.com/penrose/penrose/issues/701
  if (side.tag !== "FloatV") {
    throw new Error(
      `bboxFromSquare expected side to be FloatV, but got ${side.tag}`
    );
  }
  if (center.tag !== "VectorV") {
    throw new Error(
      `bboxFromSquare expected center to be VectorV, but got ${center.tag}`
    );
  }
  if (!isPt2(center.contents)) {
    throw new Error(
      `bboxFromSquare expected center to be Pt2, but got length ${center.contents.length}`
    );
  }

  // TODO: handle rotation, and figure out whether I need to handle rx
  return BBox.bbox(side.contents, side.contents, center.contents);
};

export const squareDef: ShapeDef = {
  shapeType: "Square",
  properties: {
    center: ["VectorV", vectorSampler],
    side: ["FloatV", widthSampler],
    rotation: ["FloatV", constValue("FloatV", 0)],
    style: ["StrV", constValue("StrV", "none")],
    rx: ["FloatV", zeroFloat],
    strokeWidth: ["FloatV", strokeSampler],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", () => noPaint],
    strokeDashArray: ["StrV", constValue("StrV", "")],
    color: ["ColorV", colorSampler],
    name: ["StrV", constValue("StrV", "defaultSquare")],
  },
  positionalProps: ["center"],
  bbox: bboxFromSquare,
};

export const textDef: ShapeDef = {
  shapeType: "Text",
  properties: {
    center: ["VectorV", vectorSampler],
    w: ["FloatV", constValue("FloatV", 0)],
    h: ["FloatV", constValue("FloatV", 0)],
    fontSize: ["StrV", constValue("StrV", "12pt")],
    rotation: ["FloatV", constValue("FloatV", 0)],
    style: ["StrV", constValue("StrV", "none")],
    stroke: ["StrV", constValue("StrV", "none")],
    color: ["ColorV", () => black],
    name: ["StrV", constValue("StrV", "defaultText")],
    string: ["StrV", constValue("StrV", "defaultLabelText")],
    // HACK: typechecking is not passing due to Value mismatch. Not sure why
  },
  positionalProps: ["center"],
  bbox: bboxFromRect, // TODO: handle rotation
};

/**
 * Preconditions:
 *   s must be axis-aligned.
 *   Assumes s is longer than it is thick.
 * Input: A line-like shape.
 * Output: A new BBox
 */
const bboxFromLinelike = ({
  start,
  end,
  thickness,
}: Properties<VarAD>): BBox.BBox => {
  // https://github.com/penrose/penrose/issues/701
  if (start.tag !== "VectorV") {
    throw new Error(
      `bboxFromLinelike expected start to be VectorV, but got ${start.tag}`
    );
  }
  if (!isPt2(start.contents)) {
    throw new Error(
      `bboxFromLinelike expected start to be Pt2, but got length ${start.contents.length}`
    );
  }
  if (end.tag !== "VectorV") {
    throw new Error(
      `bboxFromLinelike expected end to be VectorV, but got ${end.tag}`
    );
  }
  if (!isPt2(end.contents)) {
    throw new Error(
      `bboxFromLinelike expected end to be Pt2, but got length ${end.contents.length}`
    );
  }
  if (thickness.tag !== "FloatV") {
    throw new Error(
      `bboxFromLinelike expected thickness to be FloatV, but got ${thickness.tag}`
    );
  }

  // TODO: handle non-axis-aligned

  const w = max(
    absVal(sub(start.contents[0], end.contents[0])),
    thickness.contents
  );
  const h = max(
    absVal(sub(start.contents[1], end.contents[1])),
    thickness.contents
  );
  // TODO: Compute the bbox of the line in a nicer way
  const center = ops.vdiv(ops.vadd(start.contents, end.contents), constOf(2));
  if (!isPt2(center)) {
    throw new Error("ops.vadd and ops.vdiv did not preserve dimension");
  }
  return BBox.bbox(w, h, center);
};

export const lineDef: ShapeDef = {
  shapeType: "Line",
  properties: {
    start: ["VectorV", vectorSampler],
    end: ["VectorV", vectorSampler],
    thickness: ["FloatV", () => sampleFloatIn(5, 15)],
    leftArrowhead: ["BoolV", constValue("BoolV", false)],
    rightArrowhead: ["BoolV", constValue("BoolV", false)],
    arrowheadStyle: ["StrV", constValue("StrV", "arrowhead-2")],
    arrowheadSize: ["FloatV", constValue("FloatV", 1.0)],
    color: ["ColorV", colorSampler],
    style: ["StrV", constValue("StrV", "solid")],
    stroke: ["StrV", constValue("StrV", "none")],
    strokeDashArray: ["StrV", constValue("StrV", "")],
    name: ["StrV", constValue("StrV", "defaultLine")],
  },
  positionalProps: ["start", "end"],
  bbox: bboxFromLinelike,
};

export const arrowDef: ShapeDef = {
  shapeType: "Arrow",
  properties: {
    start: ["VectorV", vectorSampler],
    end: ["VectorV", vectorSampler],
    thickness: ["FloatV", () => sampleFloatIn(5, 15)],
    arrowheadStyle: ["StrV", constValue("StrV", "arrowhead-2")],
    arrowheadSize: ["FloatV", constValue("FloatV", 1.0)],
    style: ["StrV", constValue("StrV", "solid")],
    color: ["ColorV", colorSampler],
    name: ["StrV", constValue("StrV", "defaultArrow")],
    strokeDashArray: ["StrV", constValue("StrV", "")],
  },
  positionalProps: ["start", "end"],
  bbox: bboxFromLinelike,
};

const bboxFromPath = (_: Properties<VarAD>): BBox.BBox => {
  throw new Error("bboxFromPath not implemented"); // TODO
};

export const curveDef: ShapeDef = {
  shapeType: "Path",
  properties: {
    path: ["PtListV", constValue("PtListV", [])],
    polyline: ["PtListV", constValue("PtListV", [])],
    pathData: ["PathDataV", constValue("PathDataV", [])],
    strokeWidth: ["FloatV", strokeSampler],
    style: ["StrV", constValue("StrV", "solid")],
    strokeDashArray: ["StrV", constValue("StrV", "")],
    effect: ["StrV", constValue("StrV", "none")],
    color: ["ColorV", colorSampler], // should be "strokeColor"
    fill: ["ColorV", () => noPaint], // should be "color"
    leftArrowhead: ["BoolV", constValue("BoolV", false)],
    rightArrowhead: ["BoolV", constValue("BoolV", false)],
    arrowheadStyle: ["StrV", constValue("StrV", "arrowhead-2")],
    arrowheadSize: ["FloatV", constValue("FloatV", 1.0)],
    name: ["StrV", constValue("StrV", "defaultCurve")],
  },
  bbox: bboxFromPath,
};

/**
 * A registry of all types of shape definitions in the Penrose system.
 */
export const shapedefs: ShapeDef[] = [
  circleDef,
  ellipseDef,
  textDef,
  rectDef,
  calloutDef,
  polygonDef,
  freeformPolygonDef,
  polylineDef,
  pathStringDef,
  squareDef,
  curveDef,
  imageDef,
  lineDef,
  arrowDef,
];

export const positionalProps = (type: string): string[] | undefined => {
  const res = shapedefs.find(({ shapeType }: ShapeDef) => shapeType === type);
  if (!res) return undefined;
  return res.positionalProps;
};

export const findDef = (type: string): ShapeDef => {
  const res = shapedefs.find(({ shapeType }: ShapeDef) => shapeType === type);
  if (res) return res;
  else throw new Error(`${type} is not a valid shape definition.`);
};

//#endregion

//#region Shape kind queries
// Kinds of shapes
/**
 * Takes a `shapeType`, returns whether it's rectlike. (excluding squares)
 */
export const isRectlike = (shapeType: string): boolean => {
  return (
    shapeType == "Rectangle" ||
    shapeType == "Square" ||
    shapeType == "Image" ||
    shapeType == "Text"
  );
};

/**
 * Takes a `shapeType`, returns whether it's linelike.
 */
export const isLinelike = (shapeType: string): boolean => {
  return shapeType == "Line" || shapeType == "Arrow";
};
//#endregion
