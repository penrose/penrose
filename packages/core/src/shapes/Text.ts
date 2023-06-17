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
import { black, boolV, floatV, noPaint, strV, vectorV } from "../utils/Util.js";
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
  ascent: FloatV<T>;
  descent: FloatV<T>;
}

export const sampleText = (
  context: Context,
  canvas: Canvas
): TextProps<ad.Num> => ({
  name: strV("defaultText"),
  style: strV(""),
  strokeWidth: floatV(0),
  strokeStyle: strV("solid"),
  strokeColor: noPaint(),
  strokeDasharray: strV(""),
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
    })
  ),
  height: floatV(
    context.makeInput({
      init: { tag: "Pending", pending: 0 },
      stages: new Set(),
    })
  ),
  ascent: floatV(
    context.makeInput({
      init: { tag: "Pending", pending: 0 },
      stages: new Set(),
    })
  ),
  descent: floatV(
    context.makeInput({
      init: { tag: "Pending", pending: 0 },
      stages: new Set(),
    })
  ),
  rotation: floatV(0),
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

export type Text<T> = ShapeCommon<T> & { shapeType: "Text" } & TextProps<T>;

export const makeText = (
  context: Context,
  canvas: Canvas,
  properties: Partial<TextProps<ad.Num>>
): Text<ad.Num> => ({
  ...sampleText(context, canvas),
  ...properties,
  shapeType: "Text",
  passthrough: new Map(),
});
