import * as ad from "../types/ad.js";
import {
  Center,
  Fill,
  Named,
  Rect,
  Rotate,
  ShapeCommon,
  String,
  Stroke,
} from "../types/shapes.js";
import { FloatV, StrV } from "../types/value.js";
import {
  black,
  boolV,
  constStrV,
  floatV,
  noPaint,
  vectorV,
} from "../utils/Util.js";
import { Canvas, Context, uniform } from "./Samplers.js";

export interface TextProps<T>
  extends Named<T>,
    Stroke<T>,
    Fill<T>,
    Center<T>, // the center of the bounding box of the text
    Rect<T>,
    Rotate<T>,
    String<T> {
  // TODO; pare down this set of attributes
  visibility: StrV<T>;
  fontFamily: StrV<T>;
  fontSizeAdjust: StrV<T>;
  fontStretch: StrV<T>;
  fontStyle: StrV<T>;
  fontVariant: StrV<T>;
  fontWeight: StrV<T>;
  textAnchor: StrV<T>;
  lineHeight: StrV<T>;
  alignmentBaseline: StrV<T>;
  dominantBaseline: StrV<T>;
  ascent: FloatV<T>;
  descent: FloatV<T>;
}

export const sampleText = (
  context: Context,
  canvas: Canvas,
): TextProps<ad.Num> => ({
  name: constStrV("defaultText"),
  strokeWidth: floatV(0),
  strokeStyle: constStrV("solid"),
  strokeColor: noPaint(),
  strokeDasharray: constStrV(""),
  fillColor: black(),
  center: vectorV([
    context.makeInput({
      init: { tag: "Sampled", sampler: uniform(...canvas.xRange) },
      stages: "All",
    }),
    context.makeInput({
      init: { tag: "Sampled", sampler: uniform(...canvas.yRange) },
      stages: "All",
    }),
  ]),
  width: floatV(
    context.makeInput({
      init: { tag: "Pending", pending: 0 },
      stages: new Set(),
    }),
  ),
  height: floatV(
    context.makeInput({
      init: { tag: "Pending", pending: 0 },
      stages: new Set(),
    }),
  ),
  ascent: floatV(
    context.makeInput({
      init: { tag: "Pending", pending: 0 },
      stages: new Set(),
    }),
  ),
  descent: floatV(
    context.makeInput({
      init: { tag: "Pending", pending: 0 },
      stages: new Set(),
    }),
  ),
  rotation: floatV(0),
  string: constStrV("defaultText"),
  visibility: constStrV(""),
  fontFamily: constStrV("sans-serif"),
  fontSize: constStrV("12px"),
  fontSizeAdjust: constStrV(""),
  fontStretch: constStrV(""),
  fontStyle: constStrV(""),
  fontVariant: constStrV(""),
  fontWeight: constStrV(""),
  lineHeight: constStrV(""),
  textAnchor: constStrV("middle"),
  // NOTE: both `alignmentBaseline` and `dominantBaseline` are necessary for browser support. For instance, Firefox only respects the latter.
  alignmentBaseline: constStrV("alphabetic"),
  dominantBaseline: constStrV("alphabetic"),
  ensureOnCanvas: boolV(true),
});

export type Text<T> = ShapeCommon<T> & { shapeType: "Text" } & TextProps<T>;

export const makeText = (
  context: Context,
  canvas: Canvas,
  properties: Partial<TextProps<ad.Num>>,
): Text<ad.Num> => ({
  ...sampleText(context, canvas),
  ...properties,
  shapeType: "Text",
  passthrough: new Map(),
});
