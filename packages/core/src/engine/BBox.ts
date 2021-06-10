import { Pt2, VarAD } from "types/ad";
import { add, constOf, div, neg, ops } from "./Autodiff";

export class BBox {
  public readonly w: VarAD;
  public readonly h: VarAD;
  public readonly center: Pt2;
  private _corners?: BBox.Corners;

  /**
   * Input: A width, height, and center.
   * Constructs a new BBox
   */
  constructor(w: VarAD, h: VarAD, center: Pt2) {
    this.w = w;
    this.h = h;
    this.center = center;
  }

  /**
   * Input: A BBox and an inflation parameter delta.
   * Output: A new BBox inflated on all sides by delta.
   */
  static inflate(b: BBox, delta: VarAD): BBox {
    return new this(
      add(b.w, add(delta, delta)),
      add(b.h, add(delta, delta)),
      b.center
    );
  }

  /**
   * The four corners of the BBox.
   */
  get corners(): BBox.Corners {
    //   lazily create corners
    if (typeof this._corners === "undefined") {
      const halfWidth = div(this.w, constOf(2.0));
      const halfHeight = div(this.h, constOf(2.0));
      const nhalfWidth = neg(halfWidth);
      const nhalfHeight = neg(halfHeight);
      const pts = <Pt2[]>[
        [halfWidth, halfHeight],
        [nhalfWidth, halfHeight],
        [nhalfWidth, nhalfHeight],
        [halfWidth, nhalfHeight],
      ].map((p) => ops.vadd(this.center, p));

      this._corners = {
        topRight: pts[0],
        topLeft: pts[1],
        bottomLeft: pts[2],
        bottomRight: pts[3],
      };
    }

    return this._corners;
  }

  /**
   * The min X of the BBox.
   */
  get minX(): VarAD {
    return this.corners.topLeft[0];
  }

  /**
   * The max X of the BBox.
   */
  get maxX(): VarAD {
    return this.corners.bottomRight[0];
  }

  /**
   * The min Y of the BBox.
   */
  get minY(): VarAD {
    return this.corners.bottomRight[1];
  }

  /**
   * The max Y of the BBox.
   */
  get maxY(): VarAD {
    return this.corners.topLeft[1];
  }

  /**
   * The x interval of the BBox.
   */
  get xRange(): [VarAD, VarAD] {
    return [this.minX, this.maxX];
  }

  /**
   * The y interval of the BBox.
   */
  get yRange(): [VarAD, VarAD] {
    return [this.minY, this.maxY];
  }

  /**
   * The four edges of the BBox.
   */
  get edges(): BBox.Edges {
    const corners = this.corners;

    return {
      top: <[Pt2, Pt2]>[corners.topLeft, corners.topRight],
      bot: <[Pt2, Pt2]>[corners.bottomLeft, corners.bottomRight],
      left: <[Pt2, Pt2]>[corners.bottomLeft, corners.topLeft],
      right: <[Pt2, Pt2]>[corners.bottomRight, corners.topRight],
    };
  }
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

module BBox {
  export type Corners = ICorners;

  export type Intervals = IIntervals;

  export type Edges = IEdges;
}
