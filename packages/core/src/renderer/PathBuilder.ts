import * as ad from "types/ad";
import { PathDataV, SubPath } from "types/value";

/**
 * Class for building SVG paths
 */
export class PathBuilder {
  private path: PathDataV<ad.Num>;
  constructor() {
    this.path = {
      tag: "PathDataV",
      contents: [],
    };
  }
  private newCoord = (x: ad.Num, y: ad.Num): SubPath<ad.Num> => {
    return {
      tag: "CoordV",
      contents: [x, y],
    };
  };
  private newValue = (v: ad.Num[]): SubPath<ad.Num> => {
    return {
      tag: "ValueV",
      contents: v,
    };
  };

  getPath = () => this.path;

  /**
   * Moves SVG cursor to coordinate [x,y]
   */
  moveTo = ([x, y]: [ad.Num, ad.Num]) => {
    this.path.contents.push({
      cmd: "M",
      contents: [this.newCoord(x, y)],
    });
    return this;
  };
  /**
   * closes the SVG path by drawing a line between the last point and
   * the start point
   */
  closePath = () => {
    this.path.contents.push({
      cmd: "Z",
      contents: [],
    });
    return this;
  };
  /**
   * Draws a line to point [x,y]
   */
  lineTo = ([x, y]: [ad.Num, ad.Num]) => {
    this.path.contents.push({
      cmd: "L",
      contents: [this.newCoord(x, y)],
    });
    return this;
  };
  /**
   * Draws a quadratic bezier curve ending at [x,y] with one control point
   * at [cpx, cpy]
   */
  quadraticCurveTo = (
    [cpx, cpy]: [ad.Num, ad.Num],
    [x, y]: [ad.Num, ad.Num]
  ) => {
    this.path.contents.push({
      cmd: "Q",
      contents: [this.newCoord(cpx, cpy), this.newCoord(x, y)],
    });
    return this;
  };
  /**
   * Draws a cubic bezier curve ending at [x, y] with first control pt at
   * [cpx1, cpy1] and the second control pt at [cpx2, cpy2]
   */
  bezierCurveTo = (
    [cpx1, cpy1]: [ad.Num, ad.Num],
    [cpx2, cpy2]: [ad.Num, ad.Num],
    [x, y]: [ad.Num, ad.Num]
  ) => {
    this.path.contents.push({
      cmd: "C",
      contents: [
        this.newCoord(cpx1, cpy1),
        this.newCoord(cpx2, cpy2),
        this.newCoord(x, y),
      ],
    });
    return this;
  };
  /**
   * Shortcut quadratic bezier curve command ending at [x,y]
   */
  quadraticCurveJoin = ([x, y]: [ad.Num, ad.Num]) => {
    this.path.contents.push({
      cmd: "T",
      contents: [this.newCoord(x, y)],
    });
    return this;
  };
  /**
   * Shortcut cubic bezier curve command ending at [x, y]. The second control
   * pt is inferred to be the reflection of the first control pt, [cpx, cpy].
   */
  cubicCurveJoin = ([cpx, cpy]: [ad.Num, ad.Num], [x, y]: [ad.Num, ad.Num]) => {
    this.path.contents.push({
      cmd: "S",
      contents: [this.newCoord(cpx, cpy), this.newCoord(x, y)],
    });
    return this;
  };
  /**
   * Create an arc along ellipse with radius [rx, ry], ending at [x, y]
   * @param rotation: angle in degrees to rotate ellipse about its center
   * @param largeArc: 0 to draw shorter of 2 arcs, 1 to draw longer
   * @param arcSweep: 0 to rotate CCW, 1 to rotate CW
   */
  arcTo = (
    [rx, ry]: [ad.Num, ad.Num],
    [x, y]: [ad.Num, ad.Num],
    [rotation, majorArc, sweep]: ad.Num[]
  ) => {
    this.path.contents.push({
      cmd: "A",
      contents: [
        this.newValue([rx, ry, rotation, majorArc, sweep]),
        this.newCoord(x, y),
      ],
    });
    return this;
  };
}
