import { ShapeType } from "../shapes/Shapes";

export interface TypeDesc {
  description: string;
  symbol: string;
}

export const valueTypeDesc = {
  Real: { description: "Real Number", symbol: "ℝ" },
  Unit: { description: "Real Number in Unit Interval", symbol: "[0,1]" },
  PosInt: { description: "Positive Integer", symbol: "ℤ⁺" },
  Nat: { description: "Natural Number", symbol: "ℕ" },
  Real2: { description: "2d Vector of Reals", symbol: "ℝ²" },
  Real3: { description: "3d Vector of Reals", symbol: "ℝ³" },
  RealN: { description: "Vector of Reals", symbol: "ℝⁿ" },
  Real2N: { description: "List of 2d Real Vectors", symbol: "(ℝ²)ⁿ" },
  RealNM: { description: "List of Real Vectors", symbol: "(ℝⁿ)ᵐ" },
  Color: { description: "Color", symbol: "Color" },
  String: { description: "String", symbol: "String" },
  ColorType: { description: "Color Type", symbol: `"rgb" | "hsv"` },
  PathType: { description: "Path Type", symbol: `"open" | "closed"` },
  ShapeList: { description: "List of shapes", symbol: "Shape[]" },
  PathCmd: { description: "Path Command", symbol: "PathCmd" },
  Boolean: { description: "Boolean Value", symbol: `true | false` },
  ClipData: { description: "Shape clip data", symbol: "ClipData" },
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
