import { constOf } from "engine/Autodiff";
import { bboxFromLinelike } from "engine/BBox";
import { VarAD } from "types/ad";
import { IVectorV } from "types/value";
import { IArrow, INamed, IShape, IStroke, weaken } from "./Shapes";
import {
  BoolV,
  Canvas,
  FloatV,
  sampleColor,
  sampleFloatIn,
  sampleVector,
  StrV,
} from "./Samplers";

export interface ILine extends INamed, IStroke, IArrow {
  start: IVectorV<VarAD>;
  end: IVectorV<VarAD>;
}

export const sampleLine = (canvas: Canvas): ILine => ({
  name: StrV("defaultLine"),
  strokeWidth: sampleFloatIn(5, 15),
  strokeStyle: StrV("solid"),
  strokeColor: sampleColor(),
  strokeDashArray: StrV(""),
  arrowheadSize: FloatV(constOf(1)),
  arrowheadStyle: StrV("arrowhead-2"),
  startArrowhead: BoolV(false),
  endArrowhead: BoolV(false),
  start: sampleVector(canvas),
  end: sampleVector(canvas),
});

export type Line = IShape & { shapeType: "Line" } & ILine;

export const Line = {
  sampler: weaken(sampleLine),
  constr: (canvas: Canvas, properties: Partial<ILine>): Line => ({
    ...sampleLine(canvas),
    ...properties,
    shapeType: "Line",
    bbox: function () {
      return bboxFromLinelike(this);
    },
  }),
};
