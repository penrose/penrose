import { Num } from "./ad";
import { Value } from "./value";

type FunctionBody =
  | ((...args: never[]) => Value<any>)
  | ((...args: never[]) => Num);

type StyleType = {
  description: string;
  symbol: string;
};

export const styleTypes = {
  real: {
    description: "Real Number",
    symbol: "ℝ",
  },
  posint: { description: "Positive Integer", symbol: "ℤ+" },
  nat: { description: "Natural Number", symbol: "ℕ" },
  r2: { description: "2d Vector of Reals", symbol: "ℝ^2" },
  rn: { description: "Vector of Reals", symbol: "ℝ^n" },
  r2n: { description: "List of 2d Real Vectors", symbol: "(ℝ^2)^n" },
  bbox: { description: "Bounding Box", symbol: "BBox" },
  color: { description: "Color", symbol: "Color" },
  shape: {
    description: "Shape (Line | Circle | Rectangle | Text |...)",
    symbol: "Shape",
  },
  line: { description: "Line", symbol: "Line" },
  circle: { description: "Circle", symbol: "Circle" },
  rect: { description: "Rectangle", symbol: "Rect" },
};

type Argument = {
  name: string;
  description: string;
  type: keyof typeof styleTypes;
  //   for display
  default?: string;
};

export type StyleFunction = {
  documentation: string;
  name: string;
  arguments: Argument[];
  definition: FunctionBody;
};

export type Computation = StyleFunction & { returns: StyleType };
