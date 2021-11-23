import * as ShapeDef from "renderer/ShapeDef";

describe("ShapeDef", () => {
  test("circleDef.bbox", () => {
    const { bbox } = ShapeDef.circleDef;
  });

  test("ellipseDef.bbox", () => {
    const { bbox } = ShapeDef.ellipseDef;
  });

  test("rectDef.bbox", () => {
    const { bbox } = ShapeDef.rectDef;
  });

  test("calloutDef.bbox", () => {
    const { bbox } = ShapeDef.calloutDef;
  });

  test("polygonDef.bbox", () => {
    const { bbox } = ShapeDef.polygonDef;
  });

  test("freeformPolygonDef.bbox", () => {
    const { bbox } = ShapeDef.freeformPolygonDef;
  });

  test("pathStringDef.bbox", () => {
    const { bbox } = ShapeDef.pathStringDef;
  });

  test("polylineDef.bbox", () => {
    const { bbox } = ShapeDef.polylineDef;
  });

  test("imageDef.bbox", () => {
    const { bbox } = ShapeDef.imageDef;
  });

  test("squareDef.bbox", () => {
    const { bbox } = ShapeDef.squareDef;
  });

  test("textDef.bbox", () => {
    const { bbox } = ShapeDef.textDef;
  });

  test("lineDef.bbox", () => {
    const { bbox } = ShapeDef.lineDef;
  });

  test("arrowDef.bbox", () => {
    const { bbox } = ShapeDef.arrowDef;
  });

  test("curveDef.bbox", () => {
    const { bbox } = ShapeDef.curveDef;
  });
});
