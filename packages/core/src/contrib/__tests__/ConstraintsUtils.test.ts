import { ready } from "@penrose/optimizer";
import {
  overlappingAABBs,
  overlappingPolygons,
} from "contrib/ConstraintsUtils";
import { _rectangles } from "contrib/__testfixtures__/TestShapes.input";
import { genCodeSync, secondaryGraph } from "engine/Autodiff";

await ready;

describe("overlappingAABBs should return the same value as overlappingPolygons", () => {
  it.each([0, -10, -100])("padding %p", (padding: number) => {
    const t = "Rectangle";

    for (const i in _rectangles) {
      const r1 = _rectangles[i];

      for (const j in _rectangles) {
        const r2 = _rectangles[j];

        const result1 = overlappingAABBs([t, r1], [t, r2], padding);
        const result2 = overlappingPolygons([t, r1], [t, r2], padding);

        const [result1num, result2num] = genCodeSync(
          secondaryGraph([result1, result2])
        ).call([]).secondary;

        expect(result1num).toBeCloseTo(result2num, 4);
      }
    }
  });
});
