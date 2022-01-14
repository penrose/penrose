import {
  overlappingAABBs,
  overlappingPolygons,
} from "contrib/ConstraintsUtils";
import { _rectangles } from "contrib/__testfixtures__/TestShapes.input";
import { constOf, numOf } from "engine/Autodiff";

describe("overlappingAABBs should return the same value as overlappingPolygons", () => {
  it.each([0, 10, 100])("padding %p", (padding: number) => {
    const paddingAD = constOf(padding);
    const t = "Rectangle";

    for (const i in _rectangles) {
      const r1 = _rectangles[i];

      for (const j in _rectangles) {
        const r2 = _rectangles[j];

        const result1 = overlappingAABBs([t, r1], [t, r2], paddingAD);
        const result2 = overlappingPolygons([t, r1], [t, r2], paddingAD);

        expect(numOf(result1)).toBeCloseTo(numOf(result2), 4);
      }
    }
  });
});
