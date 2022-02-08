import { ICircle } from "shapes/Circle";
import { IEllipse } from "shapes/Ellipse";
import { ILine } from "shapes/Line";
import { IPath } from "shapes/Path";
import { IRectangle } from "shapes/Rectangle";
import { isPt2, Pt2, VarAD } from "types/ad";
import { ICenter, IPoly, IRect, IRotate, IScale } from "types/shapes";
import { constOf, ops } from "./Autodiff";
import {
  absVal,
  add,
  div,
  eq,
  gt,
  ifCond,
  max,
  min,
  mul,
  neg,
  sqrt,
  squared,
  sub,
} from "./AutodiffFunctions";

export interface IBBox {
  width: VarAD;
  height: VarAD;
  center: Pt2;
}

interface ICorners {
  topRight: Pt2;
  topLeft: Pt2;
  bottomLeft: Pt2;
  bottomRight: Pt2;
}

interface IIntervals {
  xRange: [VarAD, VarAD];
  yRange: [VarAD, VarAD];
}

interface IEdges {
  top: [Pt2, Pt2];
  bot: [Pt2, Pt2];
  left: [Pt2, Pt2];
  right: [Pt2, Pt2];
}

export type BBox = IBBox;

export type Corners = ICorners;

export type Intervals = IIntervals;

export type Edges = IEdges;

/**
 * Input: A width, height, and center.
 * Output: A new BBox.
 */
export const bbox = (width: VarAD, height: VarAD, center: Pt2): BBox => {
  return {
    width,
    height,
    center,
  };
};

export const corners = (b: BBox): Corners => {
  const halfWidth = div(b.width, constOf(2.0));
  const halfHeight = div(b.height, constOf(2.0));
  const nhalfWidth = neg(halfWidth);
  const nhalfHeight = neg(halfHeight);
  const pts = <Pt2[]>[
    [halfWidth, halfHeight],
    [nhalfWidth, halfHeight],
    [nhalfWidth, nhalfHeight],
    [halfWidth, nhalfHeight],
  ].map((p) => ops.vadd(b.center, p));

  return {
    topRight: pts[0],
    topLeft: pts[1],
    bottomLeft: pts[2],
    bottomRight: pts[3],
  };
};

/**
 * Input: A BBox and an inflation parameter delta.
 * Output: A BBox inflated on all sides by delta.
 */
export const inflate = (b: BBox, delta: VarAD): BBox => {
  return bbox(
    add(b.width, add(delta, delta)),
    add(b.height, add(delta, delta)),
    b.center
  );
};

/**
 * Input: A BBox.
 * Output: The min X of the BBox.
 */
export const minX = (b: BBox): VarAD => {
  return corners(b).topLeft[0];
};

/**
 * Input: A BBox.
 * Output: The max X of the BBox.
 */
export const maxX = (b: BBox): VarAD => {
  return corners(b).bottomRight[0];
};

/**
 * Input: A BBox.
 * Output: The min Y of the BBox.
 */
export const minY = (b: BBox): VarAD => {
  return corners(b).bottomRight[1];
};

/**
 * Input: A BBox.
 * Output: The max Y of the BBox.
 */
export const maxY = (b: BBox): VarAD => {
  return corners(b).topLeft[1];
};

/**
 * Input: A BBox.
 * Output: The X interval of the BBox.
 */
export const xRange = (b: BBox): [VarAD, VarAD] => {
  return [minX(b), maxX(b)];
};

/**
 * Input: A BBox.
 * Output: The Y interval of the BBox.
 */
export const yRange = (b: BBox): [VarAD, VarAD] => {
  return [minY(b), maxY(b)];
};

/**
 * Input: A BBox.
 * The four edges of the BBox.
 */
export const edges = (b: BBox): Edges => {
  return {
    top: <[Pt2, Pt2]>[corners(b).topLeft, corners(b).topRight],
    bot: <[Pt2, Pt2]>[corners(b).bottomLeft, corners(b).bottomRight],
    left: <[Pt2, Pt2]>[corners(b).bottomLeft, corners(b).topLeft],
    right: <[Pt2, Pt2]>[corners(b).bottomRight, corners(b).topRight],
  };
};

export const bboxFromPoints = (points: Pt2[]): BBox => {
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
  return bbox(w, h, center);
};

export const bboxFromRotatedRect = (
  center: Pt2,
  w: VarAD,
  h: VarAD,
  clockwise: VarAD,
  strokeWidth: VarAD
): BBox => {
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

export const bboxFromCircle = ({ r, center, strokeWidth }: ICircle): BBox => {
  // https://github.com/penrose/penrose/issues/715
  if (!isPt2(center.contents)) {
    throw new Error(
      `bboxFromCircle expected center to be Pt2, but got length ${center.contents.length}`
    );
  }

  const diameter = add(mul(constOf(2), r.contents), strokeWidth.contents);
  return bbox(diameter, diameter, center.contents);
};

export const bboxFromEllipse = ({
  rx,
  ry,
  center,
  strokeWidth,
}: IEllipse): BBox => {
  // https://github.com/penrose/penrose/issues/715
  if (!isPt2(center.contents)) {
    throw new Error(
      `bboxFromEllipse expected center to be Pt2, but got length ${center.contents.length}`
    );
  }

  const dx = mul(constOf(2), rx.contents);
  const dy = mul(constOf(2), ry.contents);
  return bbox(
    add(dx, strokeWidth.contents),
    add(dy, strokeWidth.contents),
    center.contents
  );
};

export const bboxFromRect = ({
  width,
  height,
  center,
  strokeWidth,
}: IRectangle): BBox => {
  // https://github.com/penrose/penrose/issues/715
  if (!isPt2(center.contents)) {
    throw new Error(
      `bboxFromRect expected center to be Pt2, but got length ${center.contents.length}`
    );
  }

  // rx just rounds the corners, doesn't change the bbox
  return bbox(
    add(width.contents, strokeWidth.contents),
    add(height.contents, strokeWidth.contents),
    center.contents
  );
};

export const bboxFromRectlike = ({
  center,
  width,
  height,
  rotation,
}: ICenter & IRect & IRotate): BBox => {
  // https://github.com/penrose/penrose/issues/715
  if (!isPt2(center.contents)) {
    throw new Error(
      `bboxFromRectlike expected center to be Pt2, but got length ${center.contents.length}`
    );
  }

  return bboxFromRotatedRect(
    center.contents,
    width.contents,
    height.contents,
    rotation.contents,
    constOf(0)
  );
};

export const bboxFromPolygon = ({ points, scale }: IPoly & IScale): BBox => {
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

export const bboxFromLinelike = ({ start, end, strokeWidth }: ILine): BBox => {
  // https://github.com/penrose/penrose/issues/715
  if (!isPt2(start.contents)) {
    throw new Error(
      `bboxFromLinelike expected start to be Pt2, but got length ${start.contents.length}`
    );
  }
  if (!isPt2(end.contents)) {
    throw new Error(
      `bboxFromLinelike expected end to be Pt2, but got length ${end.contents.length}`
    );
  }

  const d = ops.vmul(
    div(strokeWidth.contents, constOf(2)),
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

export const bboxFromPath = ({ d }: IPath): BBox => {
  const p = d.contents;
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
