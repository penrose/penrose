import * as ad from "types/ad";
import {
  Center,
  Fill,
  Named,
  Rect,
  Rotate,
  Shape,
  String,
  Stroke,
} from "types/shapes";
import { FloatV, StrV } from "types/value";
import {
  boolV,
  Canvas,
  sampleColor,
  sampleNoPaint,
  sampleVector,
  sampleZero,
  strV,
} from "./Samplers";

export interface TextProps
  extends Named,
    Stroke,
    Fill,
    Center, // the center of the bounding box of the text
    Rect,
    Rotate,
    String {
  // TODO; pare down this set of attributes
  visibility: StrV;
  fontFamily: StrV;
  fontSizeAdjust: StrV;
  fontStretch: StrV;
  fontStyle: StrV;
  fontVariant: StrV;
  fontWeight: StrV;
  textAnchor: StrV;
  lineHeight: StrV;
  alignmentBaseline: StrV;
  dominantBaseline: StrV;
  ascent: FloatV<ad.Num>;
  descent: FloatV<ad.Num>;
}

export const sampleText = (
  rng: seedrandom.prng,
  canvas: Canvas
): TextProps => ({
  name: strV("defaultText"),
  style: strV(""),
  strokeWidth: sampleZero(),
  strokeStyle: strV("solid"),
  strokeColor: sampleNoPaint(),
  strokeDasharray: strV(""),
  fillColor: sampleColor(rng),
  center: sampleVector(rng, canvas),
  width: sampleZero(),
  height: sampleZero(),
  ascent: sampleZero(),
  descent: sampleZero(),
  rotation: sampleZero(),
  string: strV("defaultText"),
  visibility: strV(""),
  fontFamily: strV("sans-serif"),
  fontSize: strV("12px"),
  fontSizeAdjust: strV(""),
  fontStretch: strV(""),
  fontStyle: strV(""),
  fontVariant: strV(""),
  fontWeight: strV(""),
  lineHeight: strV(""),
  textAnchor: strV("middle"),
  // NOTE: both `alignmentBaseline` and `dominantBaseline` are necessary for browser support. For instance, Firefox only respects the latter.
  alignmentBaseline: strV("alphabetic"),
  dominantBaseline: strV("alphabetic"),
  ensureOnCanvas: boolV(true),
});

export type Text = Shape & { shapeType: "Text" } & TextProps;

export const makeText = (
  rng: seedrandom.prng,
  canvas: Canvas,
  properties: Partial<TextProps>
): Text => ({
  ...sampleText(rng, canvas),
  ...properties,
  shapeType: "Text",
});
