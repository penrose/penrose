import { ShapeType } from "../shapes/Shapes";

export interface TypeDesc {
  description: string;
  symbol: string;
}

export const valueTypeDesc = {
  Real: { description: "Real Number", symbol: "ℝ" },
  Unit: { description: "Real Number in Unit Interval", symbol: "[0,1]" },
  PosInt: { description: "Positive Integer", symbol: "ℤ+" },
  Nat: { description: "Natural Number", symbol: "ℕ" },
  Real2: { description: "2d Vector of Reals", symbol: "ℝ^2" },
  RealN: { description: "Vector of Reals", symbol: "ℝ^n" },
  Real2N: { description: "List of 2d Real Vectors", symbol: "(ℝ^2)^n" },
  RealNM: { description: "List of Real Vectors", symbol: "(ℝ^n)^m" },
  Color: { description: "Color", symbol: "Color" },
  String: { description: "String", symbol: "String" },
  ColorType: { description: "Color Type", symbol: `"rgb" | "hsv"` },
  PathType: { description: "Path Type", symbol: `"open" | "closed"` },
  ShapeList: { description: "List of shapes", symbol: "Shape[]" },
  PathCmd: { description: "Path Command", symbol: "PathCmd" },
  Boolean: { description: "Boolean Value", symbol: `true | false` },
};

export type ValueType = keyof typeof valueTypeDesc;

export interface ValueT {
  tag: "ValueT";
  type: ValueType;
}

export interface ShapeT {
  tag: "ShapeT";
  type: ShapeType | "AnyShape";
}

export interface UnionT {
  tag: "UnionT";
  types: ValueShapeT[];
}

export type ValueShapeT = ValueT | ShapeT | UnionT;
