import { input } from "engine/Autodiff";
import { add, div, maxN, minN, sub } from "engine/AutodiffFunctions";
import * as BBox from "engine/BBox";
import * as ad from "types/ad";
import { Value } from "types/value";
import { Circle, makeCircle, sampleCircle } from "./Circle";
import { Ellipse, makeEllipse, sampleEllipse } from "./Ellipse";
import { Equation, makeEquation, sampleEquation } from "./Equation";
import { Group, GroupProps, makeGroup, sampleGroup } from "./Group";
import { Image, makeImage, sampleImage } from "./Image";
import { Line, makeLine, sampleLine } from "./Line";
import { makePath, Path, samplePath } from "./Path";
import { makePolygon, Polygon, samplePolygon } from "./Polygon";
import { makePolyline, Polyline, samplePolyline } from "./Polyline";
import { makeRectangle, Rectangle, sampleRectangle } from "./Rectangle";
import { Canvas, Context, InputMeta, makeCanvas } from "./Samplers";
import { makeText, sampleText, Text } from "./Text";

//#region other shape types/globals

// TODO: fix this type, it's too restrictive
export interface Properties {
  [k: string]: Value<ad.Num>;
}
BBox.bboxFromCircle;

export type Shape =
  | Circle
  | Ellipse
  | Equation
  | Image
  | Line
  | Path
  | Polygon
  | Polyline
  | Rectangle
  | Text
  | Group;

export type ShapeType = Shape["shapeType"];

export interface ShapeDef {
  sampler: (context: Context, canvas: Canvas) => Properties;
  constr: (context: Context, canvas: Canvas, properties: Properties) => Shape;

  // TODO: maybe get rid of this?
  propTags: { [prop: string]: Value<ad.Num>["tag"] };

  // TODO: make these methods
  bbox: (properties: Properties) => BBox.BBox;
  isLinelike: boolean; // TODO: use type predicate instead
  isRectlike: boolean; // TODO: remove this
  isPolygonlike: boolean; // TODO: remove this
  pendingProps: string[];
}

// hack to satisfy the typechecker
export const ShapeDef = (shapedef: {
  sampler: (context: Context, canvas: Canvas) => unknown;
  constr: (context: Context, canvas: Canvas, properties: Properties) => Shape;
  bbox: (properties: any) => BBox.BBox;
  isLinelike?: boolean;
  isRectlike?: boolean;
  isPolygonlike?: boolean;
}): ShapeDef => {
  const sampler = (context: Context, canvas: Canvas) =>
    shapedef.sampler(context, canvas) as Properties;

  const metas: InputMeta[] = [];
  const makeInput = (meta: InputMeta) => {
    const x = input({ key: metas.length, val: 0 });
    metas.push(meta);
    return x;
  };

  const ideal = sampler({ makeInput }, makeCanvas(0, 0));

  const propTags = Object.fromEntries(
    Object.entries(ideal).map(([x, y]) => [x, y.tag])
  );

  const pendingProps = [];
  for (const [key, value] of Object.entries(ideal)) {
    if (
      value.tag === "FloatV" &&
      typeof value.contents !== "number" &&
      value.contents.tag === "Input" &&
      metas[value.contents.key].init.tag === "Pending"
    ) {
      pendingProps.push(key);
    }
  }

  return {
    sampler,
    constr: shapedef.constr,

    propTags,

    bbox: shapedef.bbox,
    isLinelike: shapedef.isLinelike ?? false,
    isRectlike: shapedef.isRectlike ?? false,
    isPolygonlike: shapedef.isPolygonlike ?? false,
    pendingProps,
  };
};

//#endregion

//#region shape defs
const Circle = ShapeDef({
  sampler: sampleCircle,
  constr: makeCircle,

  bbox: BBox.bboxFromCircle,
});

const Ellipse = ShapeDef({
  sampler: sampleEllipse,
  constr: makeEllipse,

  bbox: BBox.bboxFromEllipse,
});

const Equation = ShapeDef({
  sampler: sampleEquation,
  constr: makeEquation,

  bbox: BBox.bboxFromRectlike,
  isRectlike: true,
  isPolygonlike: true,
});

const Image = ShapeDef({
  sampler: sampleImage,
  constr: makeImage,

  bbox: BBox.bboxFromRectlike, // https://github.com/penrose/penrose/issues/712
  isRectlike: true,
  isPolygonlike: true,
});

const Line = ShapeDef({
  sampler: sampleLine,
  constr: makeLine,

  bbox: BBox.bboxFromLinelike,
  isLinelike: true,
  isPolygonlike: true,
});

const Path = ShapeDef({
  sampler: samplePath,
  constr: makePath,

  bbox: BBox.bboxFromPath,
});

const Polygon = ShapeDef({
  sampler: samplePolygon,
  constr: makePolygon,

  bbox: BBox.bboxFromPolygon, // https://github.com/penrose/penrose/issues/709
  isPolygonlike: true,
});

const Polyline = ShapeDef({
  sampler: samplePolyline,
  constr: makePolyline,

  bbox: BBox.bboxFromPolygon, // https://github.com/penrose/penrose/issues/709
});

const Rectangle = ShapeDef({
  sampler: sampleRectangle,
  constr: makeRectangle,

  bbox: BBox.bboxFromRect,
  isRectlike: true,
  isPolygonlike: true,
});

const Text = ShapeDef({
  sampler: sampleText,
  constr: makeText,

  bbox: BBox.bboxFromRectlike, // assumes w and h correspond to string
  isRectlike: true,
  isPolygonlike: true,
});

// bboxFromGroup is here, not in engine/BBox, to prevent cyclic import
const bboxFromGroup = ({ shapes }: GroupProps): BBox.BBox => {
  const content = shapes.contents;
  const bboxes = content.map((gpi) => {
    const shapeType = gpi.contents[0];
    const rawShapeProps = gpi.contents[1];
    if (!isShapeType(shapeType)) {
      throw new Error("Unknown shape in Group bbox: " + shapeType);
    } else {
      const shapedef = shapedefs[shapeType];
      const shapeProps: Properties = {};
      const propKeys = Object.keys(shapedef.propTags);
      for (const prop in propKeys) {
        shapeProps[propKeys[prop]] = rawShapeProps[propKeys[prop]];
      }

      return shapedef.bbox(shapeProps);
    }
  });
  const xRanges = bboxes.map(BBox.xRange);
  const yRanges = bboxes.map(BBox.yRange);
  const minX = minN(xRanges.map((xRange) => xRange[0]));
  const maxX = maxN(xRanges.map((xRange) => xRange[1]));
  const minY = minN(yRanges.map((yRange) => yRange[0]));
  const maxY = maxN(yRanges.map((yRange) => yRange[1]));
  const width = sub(maxX, minX);
  const height = sub(maxY, minY);
  const centerX = div(add(minX, maxX), 2);
  const centerY = div(add(minY, maxY), 2);
  return BBox.bbox(width, height, [centerX, centerY]);
};

const Group = ShapeDef({
  sampler: sampleGroup,
  constr: makeGroup,

  bbox: bboxFromGroup,
});

// TODO: figure out how to not have the result be type `any` when indexing into
// this object using a string key
export const shapedefs: { [k in ShapeType]: ShapeDef } = {
  Circle,
  Ellipse,
  Equation,
  Image,
  Line,
  Path,
  Polygon,
  Polyline,
  Rectangle,
  Text,
  Group,
};

// TODO: don't use a type predicate for this
export const isShapeType = (shapeType: string): shapeType is ShapeType =>
  shapeType in shapedefs;

//#endregion
