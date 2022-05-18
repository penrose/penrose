import * as BBox from "engine/BBox";
import seedrandom from "seedrandom";
import * as ad from "types/ad";
import { Value } from "types/value";
import { Circle, makeCircle, sampleCircle } from "./Circle";
import { Ellipse, makeEllipse, sampleEllipse } from "./Ellipse";
import { Equation, makeEquation, sampleEquation } from "./Equation";
import { Image, makeImage, sampleImage } from "./Image";
import { Line, makeLine, sampleLine } from "./Line";
import { makePath, Path, samplePath } from "./Path";
import { makePolygon, Polygon, samplePolygon } from "./Polygon";
import { makePolyline, Polyline, samplePolyline } from "./Polyline";
import { makeRectangle, Rectangle, sampleRectangle } from "./Rectangle";
import { Canvas, makeCanvas } from "./Samplers";
import { makeText, sampleText, Text } from "./Text";

//#region other shape types/globals

// TODO: fix this type, it's too restrictive
export interface Properties {
  [k: string]: Value<ad.Num>;
}

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
  | Text;

export type ShapeType = Shape["shapeType"];

export interface ShapeDef {
  sampler: (rng: seedrandom.prng, canvas: Canvas) => Properties;
  constr: (
    rng: seedrandom.prng,
    canvas: Canvas,
    properties: Properties
  ) => Shape;

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
  sampler: (rng: seedrandom.prng, canvas: Canvas) => unknown;
  constr: (
    rng: seedrandom.prng,
    canvas: Canvas,
    properties: Properties
  ) => Shape;
  bbox: (properties: any) => BBox.BBox;
  isLinelike?: boolean;
  isRectlike?: boolean;
  isPolygonlike?: boolean;
  pendingProps?: string[];
}): ShapeDef => {
  const sampler = (rng: seedrandom.prng, canvas: Canvas) =>
    <Properties>shapedef.sampler(rng, canvas);

  const size = 19; // greater than 3*6; see randFloat usage in Samplers.ts
  const propTags = Object.fromEntries(
    // TODO: make this much less jank than first sampling an entire shape
    Object.entries(
      sampler(seedrandom("propTags"), makeCanvas(size, size))
    ).map(([x, y]) => [x, y.tag])
  );

  return {
    sampler,
    constr: shapedef.constr,

    propTags,

    bbox: shapedef.bbox,
    isLinelike: shapedef.isLinelike ?? false,
    isRectlike: shapedef.isRectlike ?? false,
    isPolygonlike: shapedef.isPolygonlike ?? false,
    pendingProps: shapedef.pendingProps ?? [],
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
  pendingProps: ["width", "height"],
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
  pendingProps: ["width", "height", "ascent", "descent"],
});

// TODO: figure out how to not have the result be type `any` when indexing into
// this object using a string key
export const shapedefs: { [k in Shape["shapeType"]]: ShapeDef } = {
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
};

//#endregion
