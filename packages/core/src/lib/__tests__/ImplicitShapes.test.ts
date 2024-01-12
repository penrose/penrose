import { describe, expect, test } from "vitest";
import { makeCircle } from "../../shapes/Circle.js";
import { makeEllipse } from "../../shapes/Ellipse.js";
import { makeCanvas, simpleContext } from "../../shapes/Samplers.js";
import { black, floatV, vectorV } from "../../utils/Util.js";
import {
  circleToImplicitEllipse,
  ellipseToImplicit,
  halfPlaneToImplicit,
} from "../ImplicitShapes.js";
import { numsOf } from "../Utils.js";

describe("toImplicit", () => {
  test("halfPlaneToImplicit", async () => {
    let result = halfPlaneToImplicit(
      [
        [1, 2],
        [2, 3],
      ],
      [1, 6],
      0,
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
      1,
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
      },
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
      },
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
      },
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
      },
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
