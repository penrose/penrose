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
  r: 100
  center: (42, 121)
}
`;

const ellipseSty = `Ellipse {
  rx: 200
  ry: 100
  center: (42, 121)
}
`;

const rectSty = `Rectangle {
  center: (0, 0)
  w: 150
  h: 200
}
`;

const calloutStyNoPadding = `Callout {
  center: (-50, -100)
  w: 100
  h: 100
  anchor: (100, -200)
}
`;

const calloutStyPadding = `Callout {
  center: (-50, -100)
  w: 100
  h: 100
  anchor: (100, -200)
  padding: 20
}
`;

// https://en.wikipedia.org/wiki/Polygon#/media/File:Assorted_polygons.svg
const polygonSty = (t: string) => `${t} {
  points: [(564, 24), (733, 54), (755, 154), (693, 257), (548, 216), (571, 145), (630, 146), (617, 180), (664, 196), (701, 120), (591, 90), (528, 129)]
  scale: .5
}
`;

const pathStringSty = `PathString {
  center: (-50, 100)
  w: 100
  h: 200
  rotation: 30
}
`;

const imageSty = `Image {
  center: (0, 0)
  w: 150
  h: 200
}
`;

const squareSty = `Square {
  side: 50
  center: (30, 70)
  rotation: 30
}
`;

const lineSty = (t: string) => `${t} {
  start: (-300, 200)
  end: (100, -150)
  thickness: 50
}
`;

const pathStyQ = `Path {
  pathData: makePath((-100, 0), (100, 0), 50, 10)
}
`;

const pathStyL = `Path {
  pathData: pathFromPoints("open", [(-100, -100), (100, -50), (-50, 100)])
}
`;

const pathStyA = `Path {
  pathData: arc("open", (-50, 50), (100, -25), (200, 100), 30, 0, 0)
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

  test("calloutDef.bbox no padding", async () => {
    const shape = await getShape(calloutStyNoPadding);
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.calloutDef.bbox(shape.properties);
    expect(numOf(w)).toBeCloseTo(215);
    expect(numOf(h)).toBeCloseTo(165);
    expect(numOf(x)).toBeCloseTo(-7.5);
    expect(numOf(y)).toBeCloseTo(-117.5);
  });

  test("calloutDef.bbox padding", async () => {
    const shape = await getShape(calloutStyPadding);
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.calloutDef.bbox(shape.properties);
    expect(numOf(w)).toBeCloseTo(210);
    expect(numOf(h)).toBeCloseTo(160);
    expect(numOf(x)).toBeCloseTo(-5);
    expect(numOf(y)).toBeCloseTo(-120);
  });

  test("polygonDef.bbox", async () => {
    const shape = await getShape(polygonSty("Polygon"));
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.polygonDef.bbox(shape.properties);
    expect(numOf(w)).toBeCloseTo(113.5);
    expect(numOf(h)).toBeCloseTo(116.5);
    expect(numOf(x)).toBeCloseTo(320.75);
    expect(numOf(y)).toBeCloseTo(70.25);
  });

  test("freeformPolygonDef.bbox", async () => {
    const shape = await getShape(polygonSty("FreeformPolygon"));
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

  test("pathStringDef.bbox", async () => {
    const shape = await getShape(pathStringSty);
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.pathStringDef.bbox(shape.properties);
    expect(numOf(w)).toBeCloseTo(186.603);
    expect(numOf(h)).toBeCloseTo(223.205);
    expect(numOf(x)).toBeCloseTo(-106.699);
    expect(numOf(y)).toBeCloseTo(88.397);
  });

  test("polylineDef.bbox", async () => {
    const shape = await getShape(polygonSty("Polyline"));
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.polylineDef.bbox(shape.properties);
    expect(numOf(w)).toBeCloseTo(113.5);
    expect(numOf(h)).toBeCloseTo(116.5);
    expect(numOf(x)).toBeCloseTo(320.75);
    expect(numOf(y)).toBeCloseTo(70.25);
  });

  test("imageDef.bbox", async () => {
    const shape = await getShape(imageSty);
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.imageDef.bbox(shape.properties);
    expect(numOf(w)).toBeCloseTo(150);
    expect(numOf(h)).toBeCloseTo(200);
    expect(numOf(x)).toBeCloseTo(0);
    expect(numOf(y)).toBeCloseTo(0);
  });

  test("squareDef.bbox", async () => {
    const shape = await getShape(squareSty);
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.squareDef.bbox(shape.properties);
    expect(numOf(w)).toBeCloseTo(68.301);
    expect(numOf(h)).toBeCloseTo(68.301);
    expect(numOf(x)).toBeCloseTo(14.151);
    expect(numOf(y)).toBeCloseTo(60.849);
  });

  // no Text test because w and h seem to just be set to 0 when using getShape

  test("lineDef.bbox", async () => {
    const shape = await getShape(lineSty("Line"));
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.lineDef.bbox(shape.properties);
    expect(numOf(w)).toBeCloseTo(432.925);
    expect(numOf(h)).toBeCloseTo(387.629);
    expect(numOf(x)).toBeCloseTo(-100);
    expect(numOf(y)).toBeCloseTo(25);
  });

  test("arrowDef.bbox", async () => {
    const shape = await getShape(lineSty("Arrow"));
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.arrowDef.bbox(shape.properties);
    expect(numOf(w)).toBeCloseTo(432.925);
    expect(numOf(h)).toBeCloseTo(387.629);
    expect(numOf(x)).toBeCloseTo(-100);
    expect(numOf(y)).toBeCloseTo(25);
  });

  test("curveDef.bbox quadratic", async () => {
    const shape = await getShape(pathStyQ);
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.curveDef.bbox(shape.properties);
    expect(numOf(w)).toBeCloseTo(180);
    expect(numOf(h)).toBeCloseTo(50);
    expect(numOf(x)).toBeCloseTo(0);
    expect(numOf(y)).toBeCloseTo(-25);
  });

  test("curveDef.bbox lines", async () => {
    const shape = await getShape(pathStyL);
    const {
      w,
      h,
      center: [x, y],
    } = ShapeDef.curveDef.bbox(shape.properties);
    expect(numOf(w)).toBeCloseTo(200);
    expect(numOf(h)).toBeCloseTo(200);
    expect(numOf(x)).toBeCloseTo(0);
    expect(numOf(y)).toBeCloseTo(0);
  });

  test("curveDef.bbox arc", async () => {
    // const shape = await getShape(pathStyA);
    // const {
    //   w,
    //   h,
    //   center: [x, y],
    // } = ShapeDef.curveDef.bbox(shape.properties);
    // TODO
  });

  // no Path tests using C or T or S because nothing in contrib/Functions.ts
  // uses bezierCurveTo or quadraticCurveJoin or cubicCurveJoin from
  // renderer/PathBuilder.ts
});
