import {
  Real,
  Vec,
  add,
  and,
  div,
  fn,
  lt,
  mul,
  neg,
  select,
  sqrt,
  struct,
  sub,
} from "rose";
import * as ad from "../engine/Autodiff.js";
import { rectPts } from "../lib/Queries.js";
import { toPt } from "../lib/Utils.js";
import { CircleProps } from "../shapes/Circle.js";
import { EllipseProps } from "../shapes/Ellipse.js";
import { PathProps } from "../shapes/Path.js";
import { RectangleProps } from "../shapes/Rectangle.js";
import { Center, Poly, Rect, Rotate, Scale } from "../types/shapes.js";
import { max, min } from "./Builtins.js";

const Vec2 = Vec(2, Real);

export const BBox = struct({ width: Real, height: Real, center: Vec2 });
export type BBox = ad.FromRose<typeof BBox>;

export interface Corners {
  topRight: ad.Pt2;
  topLeft: ad.Pt2;
  bottomLeft: ad.Pt2;
  bottomRight: ad.Pt2;
}

export interface Intervals {
  xRange: [ad.Num, ad.Num];
  yRange: [ad.Num, ad.Num];
}

export interface Edges {
  top: [ad.Pt2, ad.Pt2];
  bot: [ad.Pt2, ad.Pt2];
  left: [ad.Pt2, ad.Pt2];
  right: [ad.Pt2, ad.Pt2];
}

/**
 * Input: A width, height, and center.
 * Output: A new BBox.
 */
export const bbox = (width: ad.Num, height: ad.Num, center: ad.Pt2): BBox => {
  return {
    width,
    height,
    center,
  };
};

export const corners = (b: BBox): Corners => {
  const halfWidth = ad.div(b.width, 2);
  const halfHeight = ad.div(b.height, 2);
  const nhalfWidth = ad.neg(halfWidth);
  const nhalfHeight = ad.neg(halfHeight);
  const pts = <ad.Pt2[]>[
    [halfWidth, halfHeight],
    [nhalfWidth, halfHeight],
    [nhalfWidth, nhalfHeight],
    [halfWidth, nhalfHeight],
  ].map((p) => ad.ops.vadd(b.center, p));

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
export const inflate = (b: BBox, delta: ad.Num): BBox => {
  return bbox(
    ad.add(b.width, ad.add(delta, delta)),
    ad.add(b.height, ad.add(delta, delta)),
    b.center as ad.Pt2,
  );
};

/**
 * Input: A BBox.
 * Output: The min X of the BBox.
 */
export const minX = (b: BBox): ad.Num => {
  return corners(b).topLeft[0];
};

/**
 * Input: A BBox.
 * Output: The max X of the BBox.
 */
export const maxX = (b: BBox): ad.Num => {
  return corners(b).bottomRight[0];
};

/**
 * Input: A BBox.
 * Output: The min Y of the BBox.
 */
export const minY = (b: BBox): ad.Num => {
  return corners(b).bottomRight[1];
};

/**
 * Input: A BBox.
 * Output: The max Y of the BBox.
 */
export const maxY = (b: BBox): ad.Num => {
  return corners(b).topLeft[1];
};

/**
 * Input: A BBox.
 * Output: The X interval of the BBox.
 */
export const xRange = (b: BBox): [ad.Num, ad.Num] => {
  return [minX(b), maxX(b)];
};

/**
 * Input: A BBox.
 * Output: The Y interval of the BBox.
 */
export const yRange = (b: BBox): [ad.Num, ad.Num] => {
  return [minY(b), maxY(b)];
};

/**
 * Input: A BBox.
 * The four edges of the BBox.
 */
export const edges = (b: BBox): Edges => {
  return {
    top: [corners(b).topLeft, corners(b).topRight],
    bot: [corners(b).bottomLeft, corners(b).bottomRight],
    left: [corners(b).bottomLeft, corners(b).topLeft],
    right: [corners(b).bottomRight, corners(b).topRight],
  };
};

export const bboxFromPoints = (points: ad.Pt2[]): BBox => {
  const minCorner = points.reduce((corner: ad.Pt2, point: ad.Pt2) => [
    ad.min(corner[0], point[0]),
    ad.min(corner[1], point[1]),
  ]);
  const maxCorner = points.reduce((corner: ad.Pt2, point: ad.Pt2) => [
    ad.max(corner[0], point[0]),
    ad.max(corner[1], point[1]),
  ]);
  const w = ad.sub(maxCorner[0], minCorner[0]);
  const h = ad.sub(maxCorner[1], minCorner[1]);
  const center = ad.ops.vdiv(ad.ops.vadd(minCorner, maxCorner), 2);
  if (!ad.isPt2(center)) {
    throw new Error("ops.vadd and ops.vdiv did not preserve dimension");
  }
  return bbox(w, h, center);
};

export const bboxFromRotatedRect = (
  center: ad.Pt2,
  w: ad.Num,
  h: ad.Num,
  clockwise: ad.Num,
  strokeWidth: ad.Num,
): BBox => {
  return bboxFromPoints(
    rectPts(center, ad.add(w, strokeWidth), ad.add(h, strokeWidth), clockwise),
  );
};

export const bboxFromCircle = ({
  r,
  center,
  strokeWidth,
}: CircleProps<ad.Num>): BBox => {
  // https://github.com/penrose/penrose/issues/715
  if (!ad.isPt2(center.contents)) {
    throw new Error(
      `bboxFromCircle expected center to be Pt2, but got length ${center.contents.length}`,
    );
  }

  const diameter = ad.add(ad.mul(2, r.contents), strokeWidth.contents);
  return bbox(diameter, diameter, center.contents);
};

export const bboxFromEllipse = ({
  rx,
  ry,
  center,
  strokeWidth,
}: EllipseProps<ad.Num>): BBox => {
  // https://github.com/penrose/penrose/issues/715
  if (!ad.isPt2(center.contents)) {
    throw new Error(
      `bboxFromEllipse expected center to be Pt2, but got length ${center.contents.length}`,
    );
  }

  const dx = ad.mul(2, rx.contents);
  const dy = ad.mul(2, ry.contents);
  return bbox(
    ad.add(dx, strokeWidth.contents),
    ad.add(dy, strokeWidth.contents),
    center.contents,
  );
};

export const bboxFromRect = ({
  width,
  height,
  center,
  strokeWidth,
  rotation,
}: RectangleProps<ad.Num>): BBox => {
  // https://github.com/penrose/penrose/issues/715
  if (!ad.isPt2(center.contents)) {
    throw new Error(
      `bboxFromRect expected center to be Pt2, but got length ${center.contents.length}`,
    );
  }

  // rx just rounds the corners, doesn't change the bbox
  return bboxFromRotatedRect(
    toPt(center.contents),
    width.contents,
    height.contents,
    rotation.contents,
    strokeWidth.contents,
  );
};

export const bboxFromRectlike = ({
  center,
  width,
  height,
  rotation,
}: Center<ad.Num> & Rect<ad.Num> & Rotate<ad.Num>): BBox => {
  // https://github.com/penrose/penrose/issues/715
  if (!ad.isPt2(center.contents)) {
    throw new Error(
      `bboxFromRectlike expected center to be Pt2, but got length ${center.contents.length}`,
    );
  }

  return bboxFromRotatedRect(
    center.contents,
    width.contents,
    height.contents,
    rotation.contents,
    0,
  );
};

export const bboxFromPolygon = ({
  points,
  scale,
}: Poly<ad.Num> & Scale<ad.Num>): BBox => {
  return bboxFromPoints(
    points.contents.map((point) => {
      const pt = ad.ops.vmul(scale.contents, point);
      if (ad.isPt2(pt)) {
        return pt;
      } else {
        throw new Error(
          `bboxFromPolygon expected each point to be Pt2, but got length ${point.length}`,
        );
      }
    }),
  );
};

const rot90 = fn([Vec2], Vec2, ([x, y]) => [neg(y), x]);

const vadd = fn([Vec2, Vec2], Vec2, ([x1, y1], [x2, y2]) => {
  return [add(x1, x2), add(y1, y2)];
});

const vsub = fn([Vec2, Vec2], Vec2, ([x1, y1], [x2, y2]) => {
  return [sub(x1, x2), sub(y1, y2)];
});

const vmul = fn([Real, Vec2], Vec2, (c, [x, y]) => [mul(c, x), mul(c, y)]);

const vdiv = fn([Vec2, Real], Vec2, ([x, y], c) => [div(x, c), div(y, c)]);

const vnormsq = fn([Vec2], Real, ([x, y]) => add(mul(x, x), mul(y, y)));

const vnorm = fn([Vec2], Real, (v) => sqrt(vnormsq(v)));

const vnormalize = fn([Vec2], Vec2, (v) =>
  vdiv(v, add(vnorm(v), ad.EPS_DENOM)),
);

const bboxFromFourPoints = fn([Vec(4, Vec2)], BBox, (points) => {
  let minCorner = [points[0][0], points[0][1]];
  for (let i = 1; i < 4; i++) {
    const point = points[i];
    minCorner = [min(minCorner[0], point[0]), min(minCorner[1], point[1])];
  }
  let maxCorner = [points[0][0], points[0][1]];
  for (let i = 1; i < 4; i++) {
    const point = points[i];
    maxCorner = [max(maxCorner[0], point[0]), max(maxCorner[1], point[1])];
  }
  const width = sub(maxCorner[0], minCorner[0]);
  const height = sub(maxCorner[1], minCorner[1]);
  const center = vdiv(vadd(minCorner, maxCorner), 2);
  return { width, height, center };
});

export const bboxFromLinelike = ad.fn(
  [Vec2, Vec2, Real],
  BBox,
  (start, end, strokeWidth) => {
    const d = vmul(div(strokeWidth, 2), rot90(vnormalize(vsub(end, start))));
    return bboxFromFourPoints([
      vadd(start, d),
      vsub(start, d),
      vadd(end, d),
      vsub(end, d),
    ]);
  },
);

export const bboxFromPath = ({ d }: PathProps<ad.Num>): BBox => {
  const p = d.contents;
  if (p.length < 1) {
    throw new Error("bboxFromPath expected pathData to be nonempty");
  }
  if (p[0].cmd !== "M") {
    throw new Error(
      `bboxFromPath expected first command to be M, but got ${p[0].cmd}`,
    );
  }
  const first = p[0].contents[0];
  if (first.tag !== "CoordV") {
    throw new Error(
      `bboxFromPath expected first command subpath to be CoordV, but got ${first.tag}`,
    );
  }
  if (!ad.isPt2(first.contents)) {
    throw new Error(
      `bboxFromPath expected cursor to be Pt2, but got length ${first.contents.length}`,
    );
  }
  let cursor: ad.Pt2 = first.contents;
  let control: ad.Pt2 = cursor; // used by T and S

  const points: ad.Pt2[] = [];
  for (const { cmd, contents } of p) {
    const next = cmd === "Z" ? first : contents[contents.length - 1];
    if (next.tag !== "CoordV") {
      throw new Error("bboxFromPath expected next cursor to be CoordV");
    }
    if (!ad.isPt2(next.contents)) {
      throw new Error("bboxFromPath expected next cursor to be Pt2");
    }
    let nextControl = next.contents;

    if (cmd === "M") {
      // cursor is set after this if/else sequence; nothing to do here
    } else if (cmd === "Z" || cmd === "L") {
      points.push(cursor, next.contents);
    } else if (cmd === "Q") {
      const cp = contents[0].contents;
      if (!ad.isPt2(cp)) {
        throw new Error("bboxFromPath expected Q cp to be Pt2");
      }
      points.push(cursor, cp, next.contents);
      nextControl = cp;
    } else if (cmd === "C") {
      const cp1 = contents[0].contents;
      const cp2 = contents[1].contents;
      if (!ad.isPt2(cp1)) {
        throw new Error("bboxFromPath expected C cp1 to be Pt2");
      }
      if (!ad.isPt2(cp2)) {
        throw new Error("bboxFromPath expected C cp2 to be Pt2");
      }
      points.push(cursor, cp1, cp2, next.contents);
      nextControl = cp2;
    } else if (cmd === "T") {
      const cp = ad.ops.vadd(cursor, ad.ops.vsub(cursor, control));
      if (!ad.isPt2(cp)) {
        throw new Error("ops did not preserve dimension");
      }
      points.push(cursor, cp, next.contents);
      nextControl = cp;
    } else if (cmd === "S") {
      const cp1 = ad.ops.vadd(cursor, ad.ops.vsub(cursor, control));
      const cp2 = contents[0].contents;
      if (!ad.isPt2(cp1)) {
        throw new Error("ops did not preserve dimension");
      }
      if (!ad.isPt2(cp2)) {
        throw new Error("bboxFromPath expected S cp2 to be Pt2");
      }
      points.push(cursor, cp1, cp2, next.contents);
      nextControl = cp2;
    } else if (cmd === "A") {
      const [rxRaw, ryRaw, rotation, largeArc, sweep] = contents[0].contents;
      const phi = ad.neg(rotation); // phi is counterclockwise

      // https://www.w3.org/TR/SVG/implnote.html#ArcCorrectionOutOfRangeRadii
      // note: we assume neither rxRaw nor ryRaw are zero; technically in that
      // case we should just points.push(cursor, next.contents) and then not do
      // any of these other calculations

      // eq. 6.1
      const rxPos = ad.absVal(rxRaw);
      const ryPos = ad.absVal(ryRaw);

      // https://www.w3.org/TR/SVG/implnote.html#ArcConversionEndpointToCenter
      // eq. 5.1
      const [x1Prime, y1Prime] = ad.ops.vrot(
        ad.ops.vdiv(ad.ops.vsub(cursor, next.contents), 2),
        ad.neg(phi),
      );

      // eq. 6.2
      const lambda = ad.add(
        ad.squared(ad.div(x1Prime, rxPos)),
        ad.squared(ad.div(y1Prime, ryPos)),
      );

      // eq. 6.3
      const replace = ad.gt(lambda, 1);
      const rx = ad.ifCond(replace, ad.mul(ad.sqrt(lambda), rxPos), rxPos);
      const ry = ad.ifCond(replace, ad.mul(ad.sqrt(lambda), ryPos), ryPos);

      // eq. 5.2
      const cPrime = ad.ops.vmul(
        ad.mul(
          // according to the linked doc it seems like this should be the other
          // way around, but Penrose seems to do it this way instead
          ad.ifCond(ad.eq(largeArc, sweep), 1, -1),
          ad.sqrt(
            // mathematically this radicand can never be negative, but when
            // Lambda is greater than 1, the radicand becomes very close to 0
            // and sometimes negative, so we manually clamp it to a very small
            // positive value in that case
            ad.max(
              1e-18,
              ad.div(
                ad.sub(
                  ad.sub(
                    ad.squared(ad.mul(rx, ry)),
                    ad.squared(ad.mul(rx, y1Prime)),
                  ),
                  ad.squared(ad.mul(ry, x1Prime)),
                ),
                ad.add(
                  ad.squared(ad.mul(rx, y1Prime)),
                  ad.squared(ad.mul(ry, x1Prime)),
                ),
              ),
            ),
          ),
        ),
        [
          ad.div(ad.mul(rx, y1Prime), ry),
          ad.neg(ad.div(ad.mul(ry, x1Prime), rx)),
        ],
      );

      // eq. 5.3
      const [cx, cy] = ad.ops.vadd(
        ad.ops.vrot(cPrime, phi),
        ad.ops.vdiv(ad.ops.vadd(cursor, next.contents), 2),
      );

      // very crude approach: we know that the ellipse is contained within a
      // concentric circle whose diameter is the major axis, so just use the
      // bounding box of that circle
      const r = ad.max(rx, ry);
      points.push(
        [ad.sub(cx, r), ad.sub(cy, r)],
        [ad.add(cx, r), ad.add(cy, r)],
      );
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

export const intersectBbox = ad.fn([BBox, BBox], BBox, (bbox1, bbox2) => {
  const { center: center1, width: w1, height: h1 } = bbox1;
  const [x1, y1] = center1;
  const { center: center2, width: w2, height: h2 } = bbox2;
  const [x2, y2] = center2;

  const hw1 = mul(w1, 0.5);
  const hw2 = mul(w2, 0.5);
  const hh1 = mul(h1, 0.5);
  const hh2 = mul(h2, 0.5);

  const left = max(sub(x1, hw1), sub(x2, hw2));
  const right = min(add(x1, hw1), add(x2, hw2));
  const top = min(add(y1, hh1), add(y2, hh2));
  const bottom = max(sub(y1, hh1), sub(y2, hh2));

  return select(
    and(lt(left, right), lt(bottom, top)),
    BBox,
    {
      width: sub(right, left),
      height: sub(top, bottom),
      center: [div(add(left, right), 2), div(add(top, bottom), 2)],
    },
    { width: 0, height: 0, center: [0, 0] },
  );
});
