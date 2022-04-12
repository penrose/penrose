import { VarAD } from "types/ad";
import {
  ICenter,
  IFill,
  INamed,
  IRect,
  IRotate,
  IShape,
  IString,
  IStroke,
} from "types/shapes";
import { IFloatV, IStrV } from "types/value";
import {
  BoolV,
  Canvas,
  sampleColor,
  sampleNoPaint,
  sampleVector,
  sampleZero,
  StrV,
} from "./Samplers";

export interface IText
  extends INamed,
    IStroke,
    IFill,
    ICenter, // the center of the bounding box of the text
    IRect,
    IRotate,
    IString {
  // TODO; pare down this set of attributes
  visibility: IStrV;
  fontFamily: IStrV;
  fontSizeAdjust: IStrV;
  fontStretch: IStrV;
  fontStyle: IStrV;
  fontVariant: IStrV;
  fontWeight: IStrV;
  textAnchor: IStrV;
  lineHeight: IStrV;
  alignmentBaseline: IStrV;
  dominantBaseline: IStrV;
  ascent: IFloatV<VarAD>;
  descent: IFloatV<VarAD>;
}

export const sampleText = (rng: seedrandom.prng, canvas: Canvas): IText => ({
  name: StrV("defaultText"),
  style: StrV(""),
  strokeWidth: sampleZero(),
  strokeStyle: StrV("solid"),
  strokeColor: sampleNoPaint(),
  strokeDasharray: StrV(""),
  fillColor: sampleColor(rng),
  center: sampleVector(rng, canvas),
  width: sampleZero(),
  height: sampleZero(),
  ascent: sampleZero(),
  descent: sampleZero(),
  rotation: sampleZero(),
  string: StrV("defaultText"),
  visibility: StrV(""),
  fontFamily: StrV("sans-serif"),
  fontSize: StrV("12px"),
  fontSizeAdjust: StrV(""),
  fontStretch: StrV(""),
  fontStyle: StrV(""),
  fontVariant: StrV(""),
  fontWeight: StrV(""),
  lineHeight: StrV(""),
  textAnchor: StrV("middle"),
  // NOTE: both `alignmentBaseline` and `dominantBaseline` are necessary for browser support. For instance, Firefox only respects the latter.
  alignmentBaseline: StrV("alphabetic"),
  dominantBaseline: StrV("alphabetic"),
  ensureOnCanvas: BoolV(true),
});

export type Text = IShape & { shapeType: "Text" } & IText;

export const makeText = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<IText>
): Text => ({
  ...sampleText(rng, canvas),
  ...properties,
  shapeType: "Text",
});
