import { Shape } from "types/shape";
import * as ShapeDef from "renderer/ShapeDef";
import { unsafelyUnwrap } from "utils/Error";
import { compileDomain, compileSubstance, prepareState } from "index";
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

const getShape = async (shapeSty: string): Promise<Shape> => {
  const env0 = unsafelyUnwrap(compileDomain(""));
  const [subEnv, varEnv] = unsafelyUnwrap(compileSubstance("", env0));
  const state = unsafelyUnwrap(compileStyle(makeSty(shapeSty), subEnv, varEnv));
  const { shapes } = await prepareState(state);
  expect(shapes.length).toBe(1);
  return shapes[0];
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

  test("rectDef.bbox", async () => {
    const shape = await getShape(rectSty);
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.rectDef.bbox(shape.properties);
    expect(w).toBeCloseTo(150);
    expect(h).toBeCloseTo(200);
    expect(x).toBeCloseTo(0);
    expect(y).toBeCloseTo(0);
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
