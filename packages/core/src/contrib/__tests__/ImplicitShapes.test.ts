import { ready } from "@penrose/optimizer";
import { addN, mul, polyRoots, sub } from "../../engine/AutodiffFunctions";
import { makeCircle } from "../../shapes/Circle";
import { makeEllipse } from "../../shapes/Ellipse";
import { makeCanvas, simpleContext } from "../../shapes/Samplers";
import * as ad from "../../types/ad";
import { black, floatV, vectorV, zip2 } from "../../utils/Util";
import {
  circleToImplicitEllipse,
  ellipsePolynomial,
  ellipseToImplicit,
  halfPlaneToImplicit,
  implicitEllipseFunc,
} from "../ImplicitShapes";
import { pointCandidatesEllipse } from "../Minkowski";
import { numOf, numsOf } from "../Utils";

await ready;

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
      simpleContext("ImplicitShapes.test"),
      makeCanvas(800, 700),
      {
        rx: floatV(6),
        ry: floatV(3),
        center: vectorV([-11, 22]),
        strokeWidth: floatV(0),
        strokeColor: black(),
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
      simpleContext("ImplicitShapes.test"),
      makeCanvas(800, 700),
      {
        rx: floatV(1),
        ry: floatV(7),
        center: vectorV([-11, 22]),
        strokeWidth: floatV(0),
        strokeColor: black(),
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

  test("circleToImplicitEllipse", async () => {
    let circle = makeCircle(
      simpleContext("ImplicitShapes.test"),
      makeCanvas(800, 700),
      {
        r: floatV(2),
        center: vectorV([3, 4]),
        strokeWidth: floatV(0),
        strokeColor: black(),
      }
    );
    let result = circleToImplicitEllipse(circle, 0);
    let [a, b, c, x, y] = numsOf([
      result.a,
      result.b,
      result.c,
      result.x,
      result.y,
    ]);
    expect(a).toEqual(1);
    expect(b).toEqual(1);
    expect(c).toEqual(4);
    expect(x).toEqual(3);
    expect(y).toEqual(4);
  });

  test("circleToImplicitEllipse with padding", async () => {
    let circle = makeCircle(
      simpleContext("ImplicitShapes.test"),
      makeCanvas(800, 700),
      {
        r: floatV(2),
        center: vectorV([3, 4]),
        strokeWidth: floatV(0),
        strokeColor: black(),
      }
    );
    let result = circleToImplicitEllipse(circle, 1);
    let [a, b, c, x, y] = numsOf([
      result.a,
      result.b,
      result.c,
      result.x,
      result.y,
    ]);
    expect(a).toEqual(1);
    expect(b).toEqual(1);
    expect(c).toEqual(9);
    expect(x).toEqual(3);
    expect(y).toEqual(4);
  });
});

describe("ellipsePolynomial", () => {
  const ellipse1 = { a: 0.5, b: 2, c: 100, x: -11, y: 22 };
  const ellipse2 = { a: 4, b: 0.25, c: 200, x: 33, y: -44 };

  test("ellipsePolynomial produces points on the constraint manifold", async () => {
    const poly = ellipsePolynomial(ellipse1, ellipse2);
    const roots = polyRoots(poly);
    const lambdas = zip2(roots, numsOf(roots))
      .filter(([_, rn]) => !Number.isNaN(rn))
      .map(([r, _]) => r);
    const points = lambdas.map((lambda: ad.Num) =>
      pointCandidatesEllipse(ellipse1, ellipse2, lambda)
    );
    points.forEach(function ([x, y]) {
      const result = sub(
        implicitEllipseFunc(ellipse1, x, y),
        implicitEllipseFunc(ellipse2, x, y)
      );
      expect(numOf(result)).toBeCloseTo(0, 4);
    });
  });

  test("roots of ellipsePolynomial", async () => {
    const poly = ellipsePolynomial(ellipse1, ellipse2);
    const roots = polyRoots(poly);
    const lambdas = zip2(roots, numsOf(roots))
      .filter(([_, rn]) => !Number.isNaN(rn))
      .map(([r, _]) => r);
    lambdas.forEach(function (lambda) {
      let power: ad.Num = 1;
      const powers: ad.Num[] = [power];
      for (let i = 1; i <= poly.length; i++) {
        power = mul(power, lambda);
        powers.push(power);
      }
      const result = addN(
        zip2([...poly, 1], powers).map(([c, p]) => mul(c, p))
      );
      expect(numOf(result)).toBeCloseTo(0, 4);
    });
  });
});
