import { VarAD } from "types/ad";
import { mul, constOf, squared, max, ops, div, add, EPS_DENOM, min, sub, sqrt, absVal } from 'engine/Autodiff';
import { overboxFromShape, underboxFromShape } from 'engine/BBox';
import * as BBox from 'engine/BBox';

/**
 * Computes the closest distance between A and B.
 * 
 * If a shape "has measure zero", the distance is computed to the center of the object disregarding
 * its thickness. One subtlety here is that sometimes we want circles to have measure zero (points)
 * and sometimes we don't.
 * 
 * TODO: currently if the shapes intersect, the distance will be 0. is that what we want? or maybe
 * have them go negative somehow?
 */

const isRectLike = (t: string) => ["Rectangle", "Square", "Image", "Text"].includes(t);

export const hasExactImpl = (t1: string, t2: string) => {
  /* TODO: flesh this out! */
  return true;
}

/**
 * Clamp `x` in range `[l, r]`.
 */
 const clamp = ([l, r]: number[], x: VarAD): VarAD => {
  return max(constOf(l), min(constOf(r), x));
};

/**
 * Return the distance between point `pt` and segment `[start, end]`.
 */
 const pointSegmentDist = (pt: VarAD[], [start, end]: VarAD[][]): VarAD => {
  const EPS0 = constOf(10e-3);
  const lensq = max(ops.vdistsq(start, end), EPS0); // Avoid a divide-by-0 if the line is too small

  // If line seg looks like a point, the calculation just returns (something close to) `v`
  const dir = ops.vsub(end, start);
  // t = ((p -: v) `dotv` dir) / lensq -- project vector onto line seg and normalize
  const t = div(
    ops.vdot(ops.vsub(pt, start), dir),
    add(lensq, constOf(EPS_DENOM))
  );
  const t1 = clamp([0.0, 1.0], t);

  // v +: (t' *: dir) -- walk along vector of line seg
  const pointOnLine = ops.vadd(start, ops.vmul(t1, dir));
  return ops.vdist(pt, pointOnLine);
};

/**
 * Return the distance between segments `s1` and `s2`.
 */
 const segmentSegmentDist = (s1: VarAD[][], s2: VarAD[][]): VarAD => {
  return min(
    min(
      pointSegmentDist(s1[0], s2),
      pointSegmentDist(s1[1], s2),
    ),
    min(
      pointSegmentDist(s2[0], s1),
      pointSegmentDist(s2[1], s1),
    ),
  )
};

const AABBDistance = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ) => {
    if (!isRectLike(t1) || !isRectLike(t2)) {
      throw Error(`AABBDistance expected rect-like shapes, but got ${t1} and ${t2}.`);
    }

    // https://gamedev.stackexchange.com/a/154040
    const box1 = overboxFromShape(t1, s1);
    const box2 = overboxFromShape(t2, s2);

    const outerWidth = sub(
      max(BBox.maxX(box1), BBox.maxX(box2)),
      min(BBox.minX(box1), BBox.minX(box2))
    );
    const outerHeight = sub(
      max(BBox.maxY(box1), BBox.maxY(box2)),
      min(BBox.minY(box1), BBox.minY(box2))
    );

    const innerWidth = max(constOf(0), sub(sub(outerWidth, box1.w), box2.w));
    const innerHeight = max(constOf(0), sub(sub(outerHeight, box1.h), box2.h));

    return sqrt(add(squared(innerWidth), squared(innerHeight)));
}

const AABBDistanceSquared = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ) => {
    if (!isRectLike(t1) || !isRectLike(t2)) {
      throw Error(`AABBDistanceSquared expected rect-like shapes, but got ${t1} and ${t2}.`);
    }

    // https://gamedev.stackexchange.com/a/154040
    const box1 = overboxFromShape(t1, s1);
    const box2 = overboxFromShape(t2, s2);

    const outerWidth = sub(
      max(BBox.maxX(box1), BBox.maxX(box2)),
      min(BBox.minX(box1), BBox.minX(box2))
    );
    const outerHeight = sub(
      max(BBox.maxY(box1), BBox.maxY(box2)),
      min(BBox.minY(box1), BBox.minY(box2))
    );

    const innerWidth = max(constOf(0), sub(sub(outerWidth, box1.w), box2.w));
    const innerHeight = max(constOf(0), sub(sub(outerHeight, box1.h), box2.h));

    return add(squared(innerWidth), squared(innerHeight));
}

// TODO
export const lowerBound = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ): VarAD => {
    const box = underboxFromShape(t1, s1);

    return mul(box.w, box.h);
};

export const exact = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ): VarAD => {
    if (!hasExactImpl(t1, t2)) {
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    }

    if (t1 === "Line" && t2 === "Line") {
      return segmentSegmentDist([s1.start.contents, s1.end.contents], [s2.start.contents, s2.end.contents]);
    } else if (t1 === "Line" && isRectLike(t2)) {
      // TODO: there may be a faster way to do this whole computation
      const seg = [s1.start.contents, s1.end.contents];
      const box = overboxFromShape(t2, s2);
      const boxEdges = BBox.edges(box);
      return min(
        min(
          segmentSegmentDist(seg, boxEdges.top),
          segmentSegmentDist(seg, boxEdges.bot),
        ),
        min(
          segmentSegmentDist(seg, boxEdges.left),
          segmentSegmentDist(seg, boxEdges.right),
        ),
      )
    // TODO: is there a way to have these share more code?
    } else if (isRectLike(t1) && t2 === "Line") {
      // TODO: there may be a faster way to do this whole computation
      const seg = [s2.start.contents, s2.end.contents];
      const box = overboxFromShape(t1, s1);
      const boxEdges = BBox.edges(box);
      return min(
        min(
          segmentSegmentDist(seg, boxEdges.top),
          segmentSegmentDist(seg, boxEdges.bot),
        ),
        min(
          segmentSegmentDist(seg, boxEdges.left),
          segmentSegmentDist(seg, boxEdges.right),
        ),
      )
    } else if (t1 === "Line" && t2 === "Circle") {
      return sub(pointSegmentDist(s2.center.contents, [s1.start.contents, s1.end.contents]), s2.r.contents);
    } else if (t1 === "Circle" && t2 === "Line") {
      return sub(pointSegmentDist(s1.center.contents, [s2.start.contents, s2.end.contents]), s1.r.contents);
    } else if (t1 === "Line" && t2 === "Ellipse") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Ellipse" && t2 === "Line") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Line" && t2 === "Polyline") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Polyline" && t2 === "Line") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Line" && t2 === "Path") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Path" && t2 === "Line") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (isRectLike(t1) && isRectLike(t2)) {
      return AABBDistanceSquared([t1, s1], [t2, s2]);
    } else if (isRectLike(t1) && t2 === "Circle") {
      // https://gamedev.stackexchange.com/a/44496
      // (basically a special case of AABB where one of the rectangles has no extent)
      // TODO: can I unify this with AABB?
      const box = overboxFromShape(t1, s1);
      const dx = max(constOf(0), sub(
        absVal(sub(s2.center.contents[0], box.center[0])),
        div(box.w, constOf(2))
        )
      );
      const dy = max(constOf(0), sub(
        absVal(sub(s2.center.contents[1], box.center[1])),
        div(box.h, constOf(2))
        )
      );
      return sub(sqrt(add(squared(dx), squared(dy))), s2.r.contents);
    } else if (t1 === "Circle" && isRectLike(t2)) {
      // https://gamedev.stackexchange.com/a/44496
      // (basically a special case of AABB where one of the rectangles has no extent)
      // TODO: can I unify this with AABB?
      const box = overboxFromShape(t2, s2);
      const dx = max(constOf(0), sub(
        absVal(sub(s1.center.contents[0], box.center[0])),
        div(box.w, constOf(2))
        )
      );
      const dy = max(constOf(0), sub(
        absVal(sub(s1.center.contents[1], box.center[1])),
        div(box.h, constOf(2))
        )
      );
      return sub(sqrt(add(squared(dx), squared(dy))), s1.r.contents);
    } else if (isRectLike(t1) && t2 === "Ellipse") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Ellipse" && isRectLike(t2)) {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (isRectLike(t1) && t2 === "Polyline") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Polyline" && isRectLike(t2)) {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (isRectLike(t1) && t2 === "Polygon") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Polygon" && isRectLike(t2)) {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (isRectLike(t1) && t2 === "Path") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Path" && isRectLike(t2)) {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Circle" && t2 === "Circle") {
      return max(constOf(0), sub(sub(ops.vdist(s1.center.contents, s2.center.contents), s1.r.contents), s2.r.contents));
    } else if (t1 === "Circle" && t2 === "Ellipse") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Ellipse" && t2 === "Circle") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Circle" && t2 === "Polyline") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Polyline" && t2 === "Circle") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Circle" && t2 === "Polygon") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Polygon" && t2 === "Circle") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Circle" && t2 === "Path") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Path" && t2 === "Circle") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Ellipse" && t2 === "Ellipse") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Ellipse" && t2 === "Ellipse") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Ellipse" && t2 === "Ellipse") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Ellipse" && t2 === "Polyline") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Polyline" && t2 === "Ellipse") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Ellipse" && t2 === "Polygon") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Polygon" && t2 === "Ellipse") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Ellipse" && t2 === "Path") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Path" && t2 === "Ellipse") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else if (t1 === "Path" && t2 === "Path") {
      // TODO: there may be a nice way to compute this
      throw Error(`exact closest-distance query not supported for ${t1} and ${t2}`)
    } else {
      throw Error(`expected an exact closest-distance query implementation for ${t1} and ${t2}, but did not find one`);
    }
};

// TODO
export const upperBound = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  ): VarAD => {
    const box = underboxFromShape(t1, s1);

    return mul(box.w, box.h);
};