import { bboxFromRectlike } from "engine/BBox";
import { ICenter, INamed, IRect, IRotate, IShape, IString } from "./Interfaces";

export interface IEquation extends INamed, ICenter, IRect, IRotate, IString {}

export type Equation = IShape & IEquation;

export const Equation = (properties: IEquation): Equation => ({
  ...properties,
  shapeType: "Equation",
  bbox: function () {
    return bboxFromRectlike(this); // assumes w and h correspond to string
  },
});
