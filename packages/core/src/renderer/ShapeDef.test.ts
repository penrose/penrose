import { ShapeAD } from "types/shape";
import { evalShapes } from "engine/Evaluator";
import * as ShapeDef from "renderer/ShapeDef";
import { unsafelyUnwrap } from "utils/Error";
import { compileDomain, compileSubstance, initializeMat } from "index";
import { compileStyle } from "compiler/Style";
import { numOf } from "engine/Autodiff";

const makeSty = (shapeSty: string): string =>
  `canvas {
  width = 800
  height = 700
}

global {
  global.shape = ${shapeSty}
}
`;

const getShape = async (shapeSty: string): Promise<ShapeAD> => {
  // adapted from loadProgs function in compiler/Style.test.ts
  const env0 = unsafelyUnwrap(compileDomain(""));
  const [subEnv, varEnv] = unsafelyUnwrap(compileSubstance("", env0));
  const state = unsafelyUnwrap(compileStyle(makeSty(shapeSty), subEnv, varEnv));

  // adapted from prepareState function in index.ts
  await initializeMat();
  const shapes = evalShapes({
    ...state,
    originalTranslation: state.originalTranslation,
  });

  expect(shapes.length).toBe(1);
  return shapes[0];
};

const circleSty = `Circle {
  r: 100.
  center: (42., 121.)
}
`;

const ellipseSty = `Ellipse {
  rx: 200.
  ry: 100.
  center: (42., 121.)
}
`;

const rectSty = `Rectangle {
  center: (0., 0.)
  w: 150.
  h: 200.
}
`;

const lineSty = `Line {
  start: (-300., 200.)
  end: (100., -150.)
  thickness: 50
}
`;

// https://en.wikipedia.org/wiki/Polygon#/media/File:Assorted_polygons.svg
const freeformPolygonSty = `FreeformPolygon {
  points: [(564., 24.), (733., 54.), (755., 154.), (693., 257.), (548., 216.), (571., 145.), (630., 146.), (617., 180.), (664., 196.), (701., 120.), (591., 90.), (528., 129.)]
  scale: .5
}
`;

const squareSty = `Square {
  side: 50.
  center: (30., 70.)
}
`;

describe("ShapeDef", () => {
  test("circleDef.bbox", async () => {
    const shape = await getShape(circleSty);
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.circleDef.bbox(shape.properties);
    expect(numOf(w)).toBeCloseTo(200);
    expect(numOf(h)).toBeCloseTo(200);
    expect(numOf(x)).toBeCloseTo(42);
    expect(numOf(y)).toBeCloseTo(121);
  });

  test("ellipseDef.bbox", async () => {
    const shape = await getShape(ellipseSty);
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.ellipseDef.bbox(shape.properties);
    expect(numOf(w)).toBeCloseTo(400);
    expect(numOf(h)).toBeCloseTo(200);
    expect(numOf(x)).toBeCloseTo(42);
    expect(numOf(y)).toBeCloseTo(121);
  });

  test("rectDef.bbox", async () => {
    const shape = await getShape(rectSty);
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.rectDef.bbox(shape.properties);
    expect(numOf(w)).toBeCloseTo(150);
    expect(numOf(h)).toBeCloseTo(200);
    expect(numOf(x)).toBeCloseTo(0);
    expect(numOf(y)).toBeCloseTo(0);
  });

  test("calloutDef.bbox", () => {
    const { bbox } = ShapeDef.calloutDef; // TODO
  });

  test("polygonDef.bbox", () => {
    const { bbox } = ShapeDef.polygonDef; // TODO
  });

  test("freeformPolygonDef.bbox", async () => {
    const shape = await getShape(freeformPolygonSty);
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.freeformPolygonDef.bbox(shape.properties);
    expect(numOf(w)).toBeCloseTo(113.5);
    expect(numOf(h)).toBeCloseTo(116.5);
    expect(numOf(x)).toBeCloseTo(320.75);
    expect(numOf(y)).toBeCloseTo(70.25);
  });

  test("pathStringDef.bbox", () => {
    const { bbox } = ShapeDef.pathStringDef; // TODO
  });

  test("polylineDef.bbox", () => {
    const { bbox } = ShapeDef.polylineDef; // TODO
  });

  test("imageDef.bbox", () => {
    const { bbox } = ShapeDef.imageDef; // TODO
  });

  test("squareDef.bbox", async () => {
    const shape = await getShape(squareSty);
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.squareDef.bbox(shape.properties);
    expect(numOf(w)).toBeCloseTo(50);
    expect(numOf(h)).toBeCloseTo(50);
    expect(numOf(x)).toBeCloseTo(30);
    expect(numOf(y)).toBeCloseTo(70);
  });

  test("textDef.bbox", () => {
    const { bbox } = ShapeDef.textDef; // TODO
  });

  test("lineDef.bbox", async () => {
    const shape = await getShape(lineSty);
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.lineDef.bbox(shape.properties);
    // TODO
  });

  test("arrowDef.bbox", () => {
    const { bbox } = ShapeDef.arrowDef; // TODO
  });

  test("curveDef.bbox", () => {
    const { bbox } = ShapeDef.curveDef; // TODO
  });
});
