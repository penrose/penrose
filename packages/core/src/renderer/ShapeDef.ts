import {
  absVal,
  constOf,
  max,
  ops,
  sub,
  mul,
  min,
  ifCond,
  eq,
  add,
  div,
  neg,
  squared,
  gt,
  sqrt,
} from "engine/Autodiff";
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

// Bounding box definitions ====================================================
/**
 * These methods are used by subsequent shape definitions to
 * determine an enclosing axis-aligned bounding box.  Ideally
 * these boxes should be the tightest (i.e., smallest) box
 * enclosing the visible shape---including any features like
 * stroke width.  The minimum requirement is that they at least
 * provide a conservative bounding box that contains the entire
 * shape.  For instance, determining exact bounds for a Bezier
 * curve may be difficult, but using the bounding box of the
 * control points is conservative since a Bezier curve will always
 * be contained in the convex hull of its control points.
 */

const bboxFromPoints = (points: Pt2[]): BBox.BBox => {
  const minCorner = points.reduce((corner: Pt2, point: Pt2) => [
    min(corner[0], point[0]),
    min(corner[1], point[1]),
  ]);
  const maxCorner = points.reduce((corner: Pt2, point: Pt2) => [
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

const bboxFromRotatedRect = (
  center: Pt2,
  w: VarAD,
  h: VarAD,
  clockwise: VarAD,
  strokeWidth: VarAD
): BBox.BBox => {
  const counterclockwise = neg(clockwise);
  const down = ops.vrot([constOf(0), constOf(-1)], counterclockwise);
  const right = ops.rot90(down);

  const width = add(w, strokeWidth);
  const height = add(h, strokeWidth);
  const top = ops.vmul(width, right);
  const left = ops.vmul(height, down);

  const topLeft = ops.vsub(
    [sub(center[0], div(w, constOf(2))), add(center[1], div(h, constOf(2)))],
    ops.vmul(div(strokeWidth, constOf(2)), ops.vadd(down, right))
  );
  const topRight = ops.vadd(topLeft, top);
  const botLeft = ops.vadd(topLeft, left);
  const botRight = ops.vadd(topRight, left);
  if (
    !(isPt2(topLeft) && isPt2(topRight) && isPt2(botLeft) && isPt2(botRight))
  ) {
    throw new Error("ops.vadd did not preserve dimension");
  }

  return bboxFromPoints([topLeft, topRight, botLeft, botRight]);
};

const bboxFromCircle = ({
  r,
  center,
  strokeWidth,
}: Properties<VarAD>): BBox.BBox => {
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
  if (strokeWidth.tag !== "FloatV") {
    throw new Error(
      `bboxFromCircle expected strokeWidth to be FloatV, but got ${strokeWidth.tag}`
    );
  }

  const diameter = add(mul(constOf(2), r.contents), strokeWidth.contents);
  return BBox.bbox(diameter, diameter, center.contents);
};

export const circleDef: ShapeDef = {
  shapeType: "Circle",
  properties: {
    center: ["VectorV", vectorSampler],
    r: ["FloatV", widthSampler],
    //pathLength: ["FloatV", pathLengthSampler], // part of svg spec
    strokeWidth: ["FloatV", strokeSampler],
    style: ["StrV", constValue("StrV", "")],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", (): IColorV<number> => noPaint],
    color: ["ColorV", colorSampler],
    name: ["StrV", constValue("StrV", "defaultCircle")],
  },
  positionalProps: ["center"],
  bbox: bboxFromCircle,
};

const bboxFromEllipse = ({
  rx,
  ry,
  center,
  strokeWidth,
}: Properties<VarAD>): BBox.BBox => {
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
  if (strokeWidth.tag !== "FloatV") {
    throw new Error(
      `bboxFromEllipse expected strokeWidth to be FloatV, but got ${strokeWidth.tag}`
    );
  }

  const dx = mul(constOf(2), rx.contents);
  const dy = mul(constOf(2), ry.contents);
  return BBox.bbox(
    add(dx, strokeWidth.contents),
    add(dy, strokeWidth.contents),
    center.contents
  );
};

const bboxFromRect = ({
  w,
  h,
  center,
  strokeWidth,
}: Properties<VarAD>): BBox.BBox => {
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
  if (strokeWidth.tag !== "FloatV") {
    throw new Error(
      `bboxFromRect expected strokeWidth to be FloatV, but got ${strokeWidth.tag}`
    );
  }

  // rx just rounds the corners, doesn't change the bbox
  return BBox.bbox(
    add(w.contents, strokeWidth.contents),
    add(h.contents, strokeWidth.contents),
    center.contents
  );
};

const bboxFromCallout = ({
  anchor,
  center,
  w,
  h,
  padding,
  strokeWidth,
}: Properties<VarAD>): BBox.BBox => {
  // https://github.com/penrose/penrose/issues/701
  if (anchor.tag !== "VectorV") {
    throw new Error(
      `bboxFromCallout expected anchor to be VectorV, but got ${anchor.tag}`
    );
  }
  if (!isPt2(anchor.contents)) {
    throw new Error(
      `bboxFromCallout expected anchor to be Pt2, but got length ${anchor.contents.length}`
    );
  }
  if (center.tag !== "VectorV") {
    throw new Error(
      `bboxFromCallout expected center to be VectorV, but got ${center.tag}`
    );
  }
  if (!isPt2(center.contents)) {
    throw new Error(
      `bboxFromCallout expected center to be Pt2, but got length ${center.contents.length}`
    );
  }
  if (w.tag !== "FloatV") {
    throw new Error(
      `bboxFromCallout expected w to be FloatV, but got ${w.tag}`
    );
  }
  if (h.tag !== "FloatV") {
    throw new Error(
      `bboxFromCallout expected h to be FloatV, but got ${h.tag}`
    );
  }
  if (padding.tag !== "FloatV") {
    throw new Error(
      `bboxFromCallout expected padding to be FloatV, but got ${padding.tag}`
    );
  }
  if (strokeWidth.tag !== "FloatV") {
    throw new Error(
      `bboxFromRect expected strokeWidth to be FloatV, but got ${strokeWidth.tag}`
    );
  }

  // below adapted from makeCallout function in renderer/Callout.ts

  const pad = ifCond(
    eq(padding.contents, constOf(0)),
    constOf(30),
    padding.contents
  );
  const dx = div(add(w.contents, pad), constOf(2));
  const dy = div(add(h.contents, pad), constOf(2));

  const [x, y] = center.contents;
  const [anchorX, anchorY] = anchor.contents;
  const minX = min(sub(x, dx), anchorX);
  const maxX = max(add(x, dx), anchorX);
  const minY = min(sub(y, dy), anchorY);
  const maxY = max(add(y, dy), anchorY);

  const width = sub(maxX, minX);
  const height = sub(maxY, minY);
  const cx = div(add(minX, maxX), constOf(2));
  const cy = div(add(minY, maxY), constOf(2));
  return BBox.bbox(
    add(width, strokeWidth.contents),
    add(height, strokeWidth.contents),
    [cx, cy]
  );
};

const bboxFromPolygon = ({ points, scale }: Properties<VarAD>): BBox.BBox => {
  // https://github.com/penrose/penrose/issues/701
  // seems like this should be PtListV but apparently it isn't
  if (points.tag !== "LListV") {
    throw new Error(
      `bboxFromPolygon expected points to be LListV, but got ${points.tag}`
    );
  }
  if (scale.tag !== "FloatV") {
    throw new Error(
      `bboxFromPolygon expected scale to be FloatV, but got ${scale.tag}`
    );
  }

  return bboxFromPoints(
    points.contents.map((point) => {
      const pt = ops.vmul(scale.contents, point);
      if (isPt2(pt)) {
        return pt;
      } else {
        throw new Error(
          `bboxFromPolygon expected each point to be Pt2, but got length ${point.length}`
        );
      }
    })
  );
};

const bboxFromRectlike = ({
  center,
  w,
  h,
  rotation,
}: Properties<VarAD>): BBox.BBox => {
  // https://github.com/penrose/penrose/issues/701
  if (center.tag !== "VectorV") {
    throw new Error(
      `bboxFromRectlike expected center to be VectorV, but got ${center.tag}`
    );
  }
  if (!isPt2(center.contents)) {
    throw new Error(
      `bboxFromRectlike expected center to be Pt2, but got length ${center.contents.length}`
    );
  }
  if (w.tag !== "FloatV") {
    throw new Error(
      `bboxFromRectlike expected w to be FloatV, but got ${w.tag}`
    );
  }
  if (h.tag !== "FloatV") {
    throw new Error(
      `bboxFromRectlike expected h to be FloatV, but got ${h.tag}`
    );
  }
  if (rotation.tag !== "FloatV") {
    throw new Error(
      `bboxFromRectlike expected rotation to be FloatV, but got ${rotation.tag}`
    );
  }

  return bboxFromRotatedRect(
    center.contents,
    w.contents,
    h.contents,
    rotation.contents,
    constOf(0)
  );
};

const bboxFromSquare = ({
  center,
  side,
  rotation,
  strokeWidth,
}: Properties<VarAD>): BBox.BBox => {
  // https://github.com/penrose/penrose/issues/701
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
  if (side.tag !== "FloatV") {
    throw new Error(
      `bboxFromSquare expected side to be FloatV, but got ${side.tag}`
    );
  }
  if (rotation.tag !== "FloatV") {
    throw new Error(
      `bboxFromSquare expected rotation to be FloatV, but got ${rotation.tag}`
    );
  }
  if (strokeWidth.tag !== "FloatV") {
    throw new Error(
      `bboxFromSquare expected strokeWidth to be FloatV, but got ${strokeWidth.tag}`
    );
  }

  // rx rounds the corners, so we could use it to give a smaller bbox if both
  // that and rotation are nonzero, but we don't account for that here

  return bboxFromRotatedRect(
    center.contents,
    side.contents,
    side.contents,
    rotation.contents,
    strokeWidth.contents
  );
};

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

  const d = ops.vmul(
    div(thickness.contents, constOf(2)),
    ops.rot90(ops.vnormalize(ops.vsub(end.contents, start.contents)))
  );
  return bboxFromPoints(
    [
      ops.vadd(start.contents, d),
      ops.vsub(start.contents, d),
      ops.vadd(end.contents, d),
      ops.vsub(end.contents, d),
    ].map((point) => {
      if (isPt2(point)) {
        return point;
      } else {
        throw new Error("ops did not preserve dimension");
      }
    })
  );
};

const bboxFromPath = ({ pathData }: Properties<VarAD>): BBox.BBox => {
  // https://github.com/penrose/penrose/issues/701
  if (pathData.tag !== "PathDataV") {
    throw new Error(
      `bboxFromPath expected pathData to be PathDataV, but got ${pathData.tag}`
    );
  }
  // assuming path and polyline properties are not used

  const p = pathData.contents;
  if (p.length < 1) {
    throw new Error("bboxFromPath expected pathData to be nonempty");
  }
  if (p[0].cmd !== "M") {
    throw new Error(
      `bboxFromPath expected first command to be M, but got ${p[0].cmd}`
    );
  }
  const first = p[0].contents[0];
  if (first.tag !== "CoordV") {
    throw new Error(
      `bboxFromPath expected first command subpath to be CoordV, but got ${first.tag}`
    );
  }
  if (!isPt2(first.contents)) {
    throw new Error(
      `bboxFromPath expected cursor to be Pt2, but got length ${first.contents.length}`
    );
  }
  let cursor: Pt2 = first.contents;
  let control: Pt2 = cursor; // used by T and S

  const points: Pt2[] = [];
  for (const { cmd, contents } of p) {
    const next = cmd === "Z" ? first : contents[contents.length - 1];
    if (next.tag !== "CoordV") {
      throw new Error("bboxFromPath expected next cursor to be CoordV");
    }
    if (!isPt2(next.contents)) {
      throw new Error("bboxFromPath expected next cursor to be Pt2");
    }
    let nextControl = next.contents;

    if (cmd === "M") {
      // cursor is set after this if/else sequence; nothing to do here
    } else if (cmd === "Z" || cmd === "L") {
      points.push(cursor, next.contents);
    } else if (cmd === "Q") {
      const cp = contents[0].contents;
      if (!isPt2(cp)) {
        throw new Error("bboxFromPath expected Q cp to be Pt2");
      }
      points.push(cursor, cp, next.contents);
      nextControl = cp;
    } else if (cmd === "C") {
      const cp1 = contents[0].contents;
      const cp2 = contents[1].contents;
      if (!isPt2(cp1)) {
        throw new Error("bboxFromPath expected C cp1 to be Pt2");
      }
      if (!isPt2(cp2)) {
        throw new Error("bboxFromPath expected C cp2 to be Pt2");
      }
      points.push(cursor, cp1, cp2, next.contents);
      nextControl = cp2;
    } else if (cmd === "T") {
      const cp = ops.vadd(cursor, ops.vsub(cursor, control));
      if (!isPt2(cp)) {
        throw new Error("ops did not preserve dimension");
      }
      points.push(cursor, cp, next.contents);
      nextControl = cp;
    } else if (cmd === "S") {
      const cp1 = ops.vadd(cursor, ops.vsub(cursor, control));
      const cp2 = contents[0].contents;
      if (!isPt2(cp1)) {
        throw new Error("ops did not preserve dimension");
      }
      if (!isPt2(cp2)) {
        throw new Error("bboxFromPath expected S cp2 to be Pt2");
      }
      points.push(cursor, cp1, cp2, next.contents);
      nextControl = cp2;
    } else if (cmd === "A") {
      const [rxRaw, ryRaw, rotation, largeArc, sweep] = contents[0].contents;
      const phi = neg(rotation); // phi is counterclockwise

      // https://www.w3.org/TR/SVG/implnote.html#ArcCorrectionOutOfRangeRadii
      // note: we assume neither rxRaw nor ryRaw are zero; technically in that
      // case we should just points.push(cursor, next.contents) and then not do
      // any of these other calculations

      // eq. 6.1
      const rxPos = absVal(rxRaw);
      const ryPos = absVal(ryRaw);

      // https://www.w3.org/TR/SVG/implnote.html#ArcConversionEndpointToCenter
      // eq. 5.1
      const [x1Prime, y1Prime] = ops.vrot(
        ops.vdiv(ops.vsub(cursor, next.contents), constOf(2)),
        neg(phi)
      );

      // eq. 6.2
      const lambda = add(
        squared(div(x1Prime, rxPos)),
        squared(div(y1Prime, ryPos))
      );

      // eq. 6.3
      const replace = gt(lambda, constOf(1));
      const rx = ifCond(replace, mul(sqrt(lambda), rxPos), rxPos);
      const ry = ifCond(replace, mul(sqrt(lambda), ryPos), ryPos);

      // eq. 5.2
      const cPrime = ops.vmul(
        mul(
          // according to the linked doc it seems like this should be the other
          // way around, but Penrose seems to do it this way instead
          ifCond(eq(largeArc, sweep), constOf(1), constOf(-1)),
          sqrt(
            // mathematically this radicand can never be negative, but when
            // Lambda is greater than 1, the radicand becomes very close to 0
            // and sometimes negative, so we manually clamp it to a very small
            // positive value in that case, because sqrt internally calls div on
            // the radicand, and some testing shows that passing values smaller
            // than this magic number to sqrt causes that internal call to div
            // to throw an error
            max(
              constOf(1e-18),
              div(
                sub(
                  sub(squared(mul(rx, ry)), squared(mul(rx, y1Prime))),
                  squared(mul(ry, x1Prime))
                ),
                add(squared(mul(rx, y1Prime)), squared(mul(ry, x1Prime)))
              )
            )
          )
        ),
        [div(mul(rx, y1Prime), ry), neg(div(mul(ry, x1Prime), rx))]
      );

      // eq. 5.3
      const [cx, cy] = ops.vadd(
        ops.vrot(cPrime, phi),
        ops.vdiv(ops.vadd(cursor, next.contents), constOf(2))
      );

      // very crude approach: we know that the ellipse is contained within a
      // concentric circle whose diameter is the major axis, so just use the
      // bounding box of that circle
      const r = max(rx, ry);
      points.push([sub(cx, r), sub(cy, r)], [add(cx, r), add(cy, r)]);
      // ideally we would instead do something more sophisticated, like this:
      // https://stackoverflow.com/a/65441277
    } else {
      // only commands used in render/PathBuilder.ts are supported; in
      // particular, H and V are not supported, nor are any lowercase commands
      throw new Error(`bboxFromPath got unsupported cmd ${cmd}`);
    }

    cursor = next.contents;
    control = nextControl;
  }
  return bboxFromPoints(points);
};

// Shape definitions ===========================================================

export const ellipseDef: ShapeDef = {
  shapeType: "Ellipse",
  properties: {
    center: ["VectorV", vectorSampler],
    rx: ["FloatV", widthSampler],
    ry: ["FloatV", heightSampler],
    //pathLength: ["FloatV", pathLengthSampler], // part of svg spec
    strokeWidth: ["FloatV", strokeSampler],
    style: ["StrV", constValue("StrV", "")],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", (): IColorV<number> => noPaint],
    strokeDashArray: ["StrV", constValue("StrV", "")],
    color: ["ColorV", colorSampler],
    name: ["StrV", constValue("StrV", "defaultCircle")],
  },
  positionalProps: ["center"],
  bbox: bboxFromEllipse,
};

export const rectDef: ShapeDef = {
  shapeType: "Rectangle",
  properties: {
    center: ["VectorV", vectorSampler],
    w: ["FloatV", widthSampler],
    h: ["FloatV", heightSampler],
    rx: ["FloatV", zeroFloat],
    style: ["StrV", constValue("StrV", "")],
    strokeWidth: ["FloatV", strokeSampler],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", (): IColorV<number> => noPaint],
    strokeDashArray: ["StrV", constValue("StrV", "")],
    color: ["ColorV", colorSampler],
    name: ["StrV", constValue("StrV", "defaultRect")],
  },
  positionalProps: ["center"],
  bbox: bboxFromRect,
};

export const textDef: ShapeDef = {
  shapeType: "Text",
  properties: {
    center: ["VectorV", vectorSampler],
    w: ["FloatV", constValue("FloatV", 0)],
    h: ["FloatV", constValue("FloatV", 0)],
    style: ["StrV", constValue("StrV", "")],
    visibility: ["StrV", constValue("StrV", "")],
    strokeWidth: ["FloatV", strokeSampler],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", (): IColorV<number> => noPaint],
    strokeDashArray: ["StrV", constValue("StrV", "")],
    color: ["ColorV", colorSampler],
    name: ["StrV", constValue("StrV", "defaultText")],
    string: ["StrV", constValue("StrV", "Text")],
    fontFamily: ["StrV", constValue("StrV", "")],
    fontSize: ["StrV", constValue("StrV", "12pt")],
    fontSizeAdjust: ["StrV", constValue("StrV", "")],
    fontStretch: ["StrV", constValue("StrV", "")],
    fontStyle: ["StrV", constValue("StrV", "")],
    fontVariant: ["StrV", constValue("StrV", "")],
    fontWeight: ["StrV", constValue("StrV", "")],
    textAnchor: ["StrV", constValue("StrV", "middle")],
    alignmentBaseline: ["StrV", constValue("StrV", "middle")],
    rotation: ["FloatV", constValue("FloatV", 0)],
  },
  positionalProps: ["center"],
  bbox: bboxFromRectlike, // assumes w and h correspond to string
};

export const calloutDef: ShapeDef = {
  shapeType: "Callout",
  properties: {
    anchor: ["VectorV", vectorSampler],
    center: ["VectorV", vectorSampler],
    w: ["FloatV", widthSampler],
    h: ["FloatV", heightSampler],
    padding: ["FloatV", zeroFloat], // padding around the contents of the callout box
    //rx: ["FloatV", zeroFloat], // currently unused
    strokeWidth: ["FloatV", strokeSampler],
    style: ["StrV", constValue("StrV", "")],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", (): IColorV<number> => noPaint],
    strokeDashArray: ["StrV", constValue("StrV", "")],
    color: ["ColorV", colorSampler],
    name: ["StrV", constValue("StrV", "defaultCallout")],
  },
  bbox: bboxFromCallout,
};

export const polygonDef: ShapeDef = {
  shapeType: "Polygon",
  properties: {
    strokeWidth: ["FloatV", strokeSampler],
    style: ["StrV", constValue("StrV", "")],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", (): IColorV<number> => noPaint],
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
  bbox: bboxFromPolygon, // https://github.com/penrose/penrose/issues/709
};

export const freeformPolygonDef: ShapeDef = {
  shapeType: "FreeformPolygon",
  properties: {
    strokeWidth: ["FloatV", strokeSampler],
    style: ["StrV", constValue("StrV", "")],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", (): IColorV<number> => noPaint],
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
  bbox: bboxFromPolygon,
};

const DEFAULT_PATHSTR = `M 10,30
A 20,20 0,0,1 50,30
A 20,20 0,0,1 90,30
Q 90,60 50,90
Q 10,60 10,30 z`;

export const pathStringDef: ShapeDef = {
  shapeType: "PathString",
  properties: {
    center: ["VectorV", vectorSampler],
    w: ["FloatV", widthSampler],
    h: ["FloatV", heightSampler],
    rotation: ["FloatV", constValue("FloatV", 0)],
    //opacity: ["FloatV", constValue("FloatV", 1.0)],
    strokeWidth: ["FloatV", strokeSampler],
    style: ["StrV", constValue("StrV", "")],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", colorSampler],
    color: ["ColorV", (): IColorV<number> => noPaint],
    name: ["StrV", constValue("StrV", "defaultPolygon")],
    data: ["StrV", constValue("StrV", DEFAULT_PATHSTR)],
    viewBox: ["StrV", constValue("StrV", "0 0 100 100")],
  },
  positionalProps: ["center"],
  bbox: bboxFromRectlike,
};

export const polylineDef: ShapeDef = {
  shapeType: "Polyline",
  properties: {
    strokeWidth: ["FloatV", strokeSampler],
    center: ["VectorV", vectorSampler],
    scale: ["FloatV", constValue("FloatV", 1)],
    style: ["StrV", constValue("StrV", "")],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", colorSampler],
    color: ["ColorV", (): IColorV<number> => noPaint],
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
  bbox: bboxFromPolygon, // https://github.com/penrose/penrose/issues/709
};

export const imageDef: ShapeDef = {
  shapeType: "Image",
  properties: {
    center: ["VectorV", vectorSampler],
    w: ["FloatV", widthSampler],
    h: ["FloatV", heightSampler],
    rotation: ["FloatV", constValue("FloatV", 0)],
    opacity: ["FloatV", constValue("FloatV", 1.0)],
    style: ["StrV", constValue("StrV", "")],
    stroke: ["StrV", constValue("StrV", "none")],
    path: ["StrV", constValue("StrV", "missing image path")],
    name: ["StrV", constValue("StrV", "defaultImage")],
  },
  positionalProps: ["center"],
  bbox: bboxFromRectlike, // https://github.com/penrose/penrose/issues/712
};

export const squareDef: ShapeDef = {
  shapeType: "Square",
  properties: {
    center: ["VectorV", vectorSampler],
    side: ["FloatV", widthSampler],
    rotation: ["FloatV", constValue("FloatV", 0)],
    style: ["StrV", constValue("StrV", "")],
    rx: ["FloatV", zeroFloat],
    strokeWidth: ["FloatV", strokeSampler],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeColor: ["ColorV", (): IColorV<number> => noPaint],
    strokeDashArray: ["StrV", constValue("StrV", "")],
    color: ["ColorV", colorSampler],
    name: ["StrV", constValue("StrV", "defaultSquare")],
  },
  positionalProps: ["center"],
  bbox: bboxFromSquare,
};

export const equationDef: ShapeDef = {
  shapeType: "Equation",
  properties: {
    center: ["VectorV", vectorSampler],
    w: ["FloatV", constValue("FloatV", 0)],
    h: ["FloatV", constValue("FloatV", 0)],
    fontSize: ["StrV", constValue("StrV", "12pt")],
    rotation: ["FloatV", constValue("FloatV", 0)],
    style: ["StrV", constValue("StrV", "")],
    stroke: ["StrV", constValue("StrV", "none")],
    color: ["ColorV", (): IColorV<number> => black],
    name: ["StrV", constValue("StrV", "defaultText")],
    string: ["StrV", constValue("StrV", "defaultLabelText")],
    // HACK: typechecking is not passing due to Value mismatch. Not sure why
  },
  positionalProps: ["center"],
  bbox: bboxFromRectlike, // assumes w and h correspond to string
};

export const lineDef: ShapeDef = {
  shapeType: "Line",
  properties: {
    start: ["VectorV", vectorSampler],
    end: ["VectorV", vectorSampler],
    thickness: ["FloatV", (): IFloatV<number> => sampleFloatIn(5, 15)],
    leftArrowhead: ["BoolV", constValue("BoolV", false)],
    rightArrowhead: ["BoolV", constValue("BoolV", false)],
    arrowheadStyle: ["StrV", constValue("StrV", "arrowhead-2")],
    arrowheadSize: ["FloatV", constValue("FloatV", 1.0)],
    color: ["ColorV", colorSampler],
    style: ["StrV", constValue("StrV", "")],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    //stroke: ["StrV", constValue("StrV", "none")],
    strokeDashArray: ["StrV", constValue("StrV", "")],
    strokeLineCap: ["StrV", constValue("StrV", "")],
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
    thickness: ["FloatV", (): IFloatV<number> => sampleFloatIn(5, 15)],
    arrowheadStyle: ["StrV", constValue("StrV", "arrowhead-2")],
    arrowheadSize: ["FloatV", constValue("FloatV", 1.0)],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    color: ["ColorV", colorSampler],
    style: ["StrV", constValue("StrV", "")],
    name: ["StrV", constValue("StrV", "defaultArrow")],
    strokeDashArray: ["StrV", constValue("StrV", "")],
  },
  positionalProps: ["start", "end"],
  bbox: bboxFromLinelike,
};

export const curveDef: ShapeDef = {
  shapeType: "Path",
  properties: {
    path: ["PtListV", constValue("PtListV", [])],
    polyline: ["PtListV", constValue("PtListV", [])],
    pathData: ["PathDataV", constValue("PathDataV", [])],
    strokeWidth: ["FloatV", strokeSampler],
    style: ["StrV", constValue("StrV", "")],
    strokeStyle: ["StrV", constValue("StrV", "solid")],
    strokeDashArray: ["StrV", constValue("StrV", "")],
    effect: ["StrV", constValue("StrV", "none")],
    color: ["ColorV", colorSampler], // should be "strokeColor"
    fill: ["ColorV", (): IColorV<number> => noPaint], // should be "color"
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
  equationDef,
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
  textDef,
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
    shapeType == "Text" ||
    shapeType == "Equation"
  );
};

/**
 * Takes a `shapeType`, returns whether it's linelike.
 */
export const isLinelike = (shapeType: string): boolean => {
  return shapeType == "Line" || shapeType == "Arrow";
};
//#endregion
