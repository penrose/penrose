import * as BBox from "engine/BBox";
import { VarAD } from "types/ad";
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
import { Text, makeText, sampleText } from "./Text";

//#region other shape types/globals

// TODO: fix this type, it's too restrictive
export interface Properties {
  [k: string]: Value<VarAD>;
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

export interface ShapeDef {
  sampler: (canvas: Canvas) => Properties;
  constr: (canvas: Canvas, properties: Properties) => Shape;

  // TODO: maybe get rid of this?
  propTags: () => { [prop: string]: Value<VarAD>["tag"] };

  // TODO: make these methods
  bbox: (properties: Properties) => BBox.BBox;
  isLinelike: boolean; // TODO: use type predicate instead
  isRectlike: boolean; // TODO: remove this
}

// hack to satisfy the typechecker
export const ShapeDef = (shapedef: {
  sampler: (canvas: Canvas) => unknown;
  constr: (canvas: Canvas, properties: Properties) => Shape;
  bbox: (properties: any) => BBox.BBox;
  isLinelike?: boolean;
  isRectlike?: boolean;
}): ShapeDef => {
  const sampler = (canvas: Canvas) => <Properties>shapedef.sampler(canvas);
  return {
    sampler,
    constr: shapedef.constr,

    propTags: () => {
      const size = 19; // greater than 3*6; see randFloat usage in Samplers.ts
      return Object.fromEntries(
        // TODO: make this much less jank than first sampling an entire shape
        Object.entries(sampler(makeCanvas(size, size))).map(([x, y]) => [
          x,
          y.tag,
        ])
      );
    },

    bbox: shapedef.bbox,
    isLinelike: shapedef.isLinelike || false,
    isRectlike: shapedef.isRectlike || false,
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
});

const Image = ShapeDef({
  sampler: sampleImage,
  constr: makeImage,

  bbox: BBox.bboxFromRectlike, // https://github.com/penrose/penrose/issues/712
  isRectlike: true,
});

const Line = ShapeDef({
  sampler: sampleLine,
  constr: makeLine,

  bbox: BBox.bboxFromLinelike,
  isLinelike: true,
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
});

const Text = ShapeDef({
  sampler: sampleText,
  constr: makeText,

  bbox: BBox.bboxFromRectlike, // assumes w and h correspond to string
  isRectlike: true,
});

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
