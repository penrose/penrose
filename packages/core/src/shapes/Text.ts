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
} from "./Interfaces";

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

export type Text = IShape & IText;

export const Text = (properties: IText): Text => ({
  ...properties,
  shapeType: "Text",
  bbox: function () {
    return bboxFromRectlike(this); // // assumes w and h correspond to string
  },
});
