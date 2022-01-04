import { bboxFromRectlike } from "engine/BBox";
import { IStrV } from "types/value";
import {
  ICenter,
  IFill,
  INamed,
  IRect,
  IRotate,
  IShape,
  IString,
  IStroke,
  weaken,
} from "./Shapes";
import {
  Canvas,
  sampleColor,
  sampleNoPaint,
  sampleStroke,
  sampleVector,
  sampleZero,
  StrV,
} from "./Samplers";

export interface IText
  extends INamed,
    IStroke,
    IFill,
    ICenter,
    IRect,
    IRotate,
    IString {
  // TODO; pare down this set of attributes
  visibility: IStrV;
  fontFamily: IStrV;
  fontSize: IStrV;
  fontSizeAdjust: IStrV;
  fontStretch: IStrV;
  fontStyle: IStrV;
  fontVariant: IStrV;
  fontWeight: IStrV;
  textAnchor: IStrV;
  alignmentBaseline: IStrV;
}

export const sampleText = (canvas: Canvas): IText => ({
  name: StrV("defaultText"),
  strokeWidth: sampleStroke(),
  strokeStyle: StrV("solid"),
  strokeColor: sampleNoPaint(),
  strokeDashArray: StrV(""),
  fillColor: sampleColor(),
  center: sampleVector(canvas),
  width: sampleZero(),
  height: sampleZero(),
  rotation: sampleZero(),
  string: StrV("defaultText"),
  visibility: StrV(""),
  fontFamily: StrV(""),
  fontSize: StrV("12pt"),
  fontSizeAdjust: StrV(""),
  fontStretch: StrV(""),
  fontStyle: StrV(""),
  fontVariant: StrV(""),
  fontWeight: StrV(""),
  textAnchor: StrV("middle"),
  alignmentBaseline: StrV("middle"),
});

export type Text = IShape & IText;

export const Text = {
  sampler: weaken(sampleText),
  constr: (canvas: Canvas, properties: Partial<IText>): Text => ({
    ...sampleText(canvas),
    ...properties,
    shapeType: "Text",
    bbox: function () {
      return bboxFromRectlike(this); // // assumes w and h correspond to string
    },
  }),
};
