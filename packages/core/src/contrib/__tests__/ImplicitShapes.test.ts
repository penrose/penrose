import { ellipseToImplicit, halfPlaneToImplicit } from "contrib/ImplicitShapes";
import { numsOf } from "contrib/Utils";
import seedrandom from "seedrandom";
import { makeEllipse } from "shapes/Ellipse";
import { floatV, makeCanvas, sampleBlack, vectorV } from "shapes/Samplers";

describe("toImplicit", () => {
  test("halfPlaneToImplicit", async () => {
    let result = halfPlaneToImplicit(
      [
        [1, 2],
        [2, 3],
      ],
      [1, 6],
      0
    );
    let [a, b, c] = numsOf([result.a, result.b, result.c]);
    expect(a).toBeCloseTo(1 / Math.sqrt(2), 4);
    expect(b).toBeCloseTo(-1 / Math.sqrt(2), 4);
    expect(c).toBeCloseTo(-1 / Math.sqrt(2), 4);
  });

  test("halfPlaneToImplicit with padding", async () => {
    let result = halfPlaneToImplicit(
      [
        [1, 2],
        [3, 4],
      ],
      [5, 6],
      1
    );
    let [a, b, c] = numsOf([result.a, result.b, result.c]);
    expect(a).toBeCloseTo(1 / Math.sqrt(2), 4);
    expect(b).toBeCloseTo(-1 / Math.sqrt(2), 4);
    expect(c).toBeCloseTo(-1 / Math.sqrt(2) - 1, 4);
  });

  test("ellipseToImplicit", async () => {
    let ellipse = makeEllipse(
      seedrandom("ImplicitShapes.test"),
      makeCanvas(800, 700),
      {
        rx: floatV(6),
        ry: floatV(3),
        center: vectorV([-11, 22]),
        strokeWidth: floatV(0),
        strokeColor: sampleBlack(),
      }
    );
    let result = ellipseToImplicit(ellipse, 0);
    let [a, b, c, x, y] = numsOf([
      result.a,
      result.b,
      result.c,
      result.x,
      result.y,
    ]);
    expect(a).toEqual(0.5);
    expect(b).toEqual(2);
    expect(c).toEqual(18);
    expect(x).toEqual(-11);
    expect(y).toEqual(22);
  });

  test("ellipseToImplicit with padding", async () => {
    let ellipse = makeEllipse(
      seedrandom("ImplicitShapes.test"),
      makeCanvas(800, 700),
      {
        rx: floatV(1),
        ry: floatV(7),
        center: vectorV([-11, 22]),
        strokeWidth: floatV(0),
        strokeColor: sampleBlack(),
      }
    );
    let result = ellipseToImplicit(ellipse, 1);
    let [a, b, c, x, y] = numsOf([
      result.a,
      result.b,
      result.c,
      result.x,
      result.y,
    ]);
    expect(a).toEqual(4);
    expect(b).toEqual(0.25);
    expect(c).toEqual(16);
    expect(x).toEqual(-11);
    expect(y).toEqual(22);
  });
});
