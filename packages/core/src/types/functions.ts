type FunctionBody<T> = (...args: any[]) => T;

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
  unit: { description: "Unit Interval", symbol: "[0,1]" },
  bbox: { description: "Bounding Box", symbol: "BBox" },
  color: { description: "Color", symbol: "Color" },
  shape: {
    description: "Shape (Line | Circle | Rectangle | Text |...)",
    symbol: "Shape",
  },
  line: { description: "Line", symbol: "Line" },
  circle: { description: "Circle", symbol: "Circle" },
  rect: { description: "Rectangle", symbol: "Rect" },
  path: { description: "Path", symbol: "Path" },
  string: { description: "String", symbol: "String" },
  stringlist: { description: "List of Strings", symbol: "String[]" },
  pathtype: { description: "Path Type", symbol: `"Open" | "Closed"` },
};

type Argument = {
  name: string;
  description: string;
  type: keyof typeof styleTypes;
  //   for display
  default?: string;
};

export type StyleFunction<T> = {
  documentation: string;
  name: string;
  arguments: Argument[];
  definition: FunctionBody<T>;
};

export type Computation<T> = StyleFunction<T> & {
  returns: keyof typeof styleTypes;
};
