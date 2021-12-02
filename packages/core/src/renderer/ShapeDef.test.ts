import { Shape } from "types/shape";
import * as ShapeDef from "renderer/ShapeDef";
import { unsafelyUnwrap } from "utils/Error";
import { compileDomain, compileSubstance } from "index";
import { compileStyle } from "compiler/Style";

const makeSty = (shapeSty: string): string =>
  `canvas {
  width = 800
  height = 700
}

global {
  global.shape = ${shapeSty}
}
`;

const getShapes = (shapeSty: string): Shape[] => {
  const env0 = unsafelyUnwrap(compileDomain(""));
  const [subEnv, varEnv] = unsafelyUnwrap(compileSubstance("", env0));
  const state = unsafelyUnwrap(compileStyle(makeSty(shapeSty), subEnv, varEnv));
  return state.shapes;
};

const rectSty = `Rectangle {
  center: (0., 0.)
  w: 150.
  h: 200.
}
`;

describe("ShapeDef", () => {
  test("circleDef.bbox", () => {
    const { bbox } = ShapeDef.circleDef;
  });

  test("ellipseDef.bbox", () => {
    const { bbox } = ShapeDef.ellipseDef;
  });

  test("rectDef.bbox", () => {
    const { bbox } = ShapeDef.rectDef;
    const shapes = getShapes(rectSty);
    expect(shapes).toEqual(["something"]);
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
