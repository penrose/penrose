import { scalar } from "@tensorflow/tfjs";
import { describe, expect, test } from "vitest";
import { addN, mul, polyRoots, sub } from "../../engine/AutodiffFunctions.js";
import { makeCircle } from "../../shapes/Circle.js";
import { makeEllipse } from "../../shapes/Ellipse.js";
import { makeCanvas, simpleContext } from "../../shapes/Samplers.js";
import * as ad from "../../types/ad.js";
import { black, floatV, vectorV, zip2 } from "../../utils/Util.js";
import {
  circleToImplicitEllipse,
  ellipsePolynomial,
  ellipseToImplicit,
  halfPlaneToImplicit,
  implicitEllipseFunc,
} from "../ImplicitShapes.js";
import { pointCandidatesEllipse } from "../Minkowski.js";
import { numOf, numsOf } from "../Utils.js";

describe("toImplicit", () => {
  test("halfPlaneToImplicit", async () => {
    let result = halfPlaneToImplicit(
      [
        [scalar(1), scalar(2)],
        [scalar(2), scalar(3)],
      ],
      [scalar(1), scalar(6)],
      scalar(0),
    );
    let [a, b, c] = numsOf([result.a, result.b, result.c]);
    expect(a).toBeCloseTo(1 / Math.sqrt(2), 4);
    expect(b).toBeCloseTo(-1 / Math.sqrt(2), 4);
    expect(c).toBeCloseTo(-1 / Math.sqrt(2), 4);
  });

  test("halfPlaneToImplicit with padding", async () => {
    let result = halfPlaneToImplicit(
      [
        [scalar(1), scalar(2)],
        [scalar(3), scalar(4)],
      ],
      [scalar(5), scalar(6)],
      scalar(1),
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
        rx: floatV(scalar(6)),
        ry: floatV(scalar(3)),
        center: vectorV([scalar(-11), scalar(22)]),
        strokeWidth: floatV(scalar(0)),
        strokeColor: black(),
      },
    );
    let result = ellipseToImplicit(ellipse, scalar(0));
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
        rx: floatV(scalar(1)),
        ry: floatV(scalar(7)),
        center: vectorV([scalar(-11), scalar(22)]),
        strokeWidth: floatV(scalar(0)),
        strokeColor: black(),
      },
    );
    let result = ellipseToImplicit(ellipse, scalar(1));
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
        r: floatV(scalar(2)),
        center: vectorV([scalar(3), scalar(4)]),
        strokeWidth: floatV(scalar(0)),
        strokeColor: black(),
      },
    );
    let result = circleToImplicitEllipse(circle, scalar(0));
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
        r: floatV(scalar(2)),
        center: vectorV([scalar(3), scalar(4)]),
        strokeWidth: floatV(scalar(0)),
        strokeColor: black(),
      },
    );
    let result = circleToImplicitEllipse(circle, scalar(1));
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
  const ellipse1 = {
    a: scalar(0.5),
    b: scalar(2),
    c: scalar(100),
    x: scalar(-11),
    y: scalar(22),
  };
  const ellipse2 = {
    a: scalar(4),
    b: scalar(0.25),
    c: scalar(200),
    x: scalar(33),
    y: scalar(-44),
  };

  test("ellipsePolynomial produces points on the constraint manifold", async () => {
    const poly = ellipsePolynomial(ellipse1, ellipse2);
    const roots = polyRoots(poly);
    const lambdas = zip2(roots, numsOf(roots))
      .filter(([_, rn]) => !Number.isNaN(rn))
      .map(([r, _]) => r);
    const points = lambdas.map((lambda: ad.Num) =>
      pointCandidatesEllipse(ellipse1, ellipse2, lambda),
    );
    points.forEach(function ([x, y]) {
      const result = sub(
        implicitEllipseFunc(ellipse1, x, y),
        implicitEllipseFunc(ellipse2, x, y),
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
      let power: ad.Num = scalar(1);
      const powers: ad.Num[] = [power];
      for (let i = 1; i <= poly.length; i++) {
        power = mul(power, lambda);
        powers.push(power);
      }
      const result = addN(
        zip2([...poly, 1], powers).map(([c, p]) => mul(c, p)),
      );
      expect(numOf(result)).toBeCloseTo(0, 4);
    });
  });
});
