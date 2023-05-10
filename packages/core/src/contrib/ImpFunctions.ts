import { Context } from "../shapes/Samplers";
import { Shape } from "../shapes/Shapes";
import * as ad from "../types/ad";
import { CompFunc } from "../types/functions";
import { ShapeListV } from "../types/value";
import { shapeListV, shapeT, valueT } from "../utils/Util";

export const impDict = {
  appendShape: {
    name: "appendShape",
    description:
      "Appends a shape into a list of shapes in-place and returns the resultant list",
    params: [
      {
        name: "shapes",
        type: valueT("ShapeList"),
        description: "A list of shapes",
      },
      { name: "shape", type: shapeT("AnyShape"), description: "A shape" },
    ],
    body: (
      _context: Context,
      shapes: Shape<ad.Num>[],
      shape: Shape<ad.Num>
    ): ShapeListV<ad.Num> => {
      shapes.push(shape);
      return shapeListV(shapes);
    },
    returns: valueT("ShapeList"),
  },
};

// `_impDictVals` causes TypeScript to enforce that every function in
// `impDict` actually has type `CompFunc` with the right function signature, etc.
const _impDictVals: CompFunc[] = Object.values(impDict);
