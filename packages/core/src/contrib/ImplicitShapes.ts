import { outwardUnitNormal } from "contrib/Queries";
import { EPS_DENOM, ops } from "engine/Autodiff";
import {
  add,
  div,
  eq,
  ifCond,
  max,
  mul,
  neg,
  squared,
  sub,
} from "engine/AutodiffFunctions";
import { Circle } from "shapes/Circle";
import { Ellipse } from "shapes/Ellipse";
import * as ad from "types/ad";

/**
 * Parameters of implicitly defined ellipse:
 * `a * (X - x)^2 + b * (Y - y)^2 = c`
 */
export interface ImplicitEllipse {
  a: ad.Num;
  b: ad.Num;
  c: ad.Num;
  x: ad.Num;
  y: ad.Num;
}

/**
 * Parameters of implicitly defined half-plane:
 * `a * X + b * Y <= c`
 */
export interface ImplicitHalfPlane {
  a: ad.Num;
  b: ad.Num;
  c: ad.Num;
}

/**
 * Evaluate the implicit function for an ellipse at point with coordinates `x` and `y`.
 * @param ei Implicit ellipse parameters.
 * @param x X-coordinate.
 * @param y Y-coordinate.
 */
export const implicitEllipseFunc = (
  ei: ImplicitEllipse,
  x: ad.Num,
  y: ad.Num
): ad.Num => {
  return sub(
    add(mul(ei.a, squared(sub(x, ei.x))), mul(ei.b, squared(sub(y, ei.y)))),
    ei.c
  );
};

/**
 * Evaluate the implicit function for an intersection of 2 ellipses at point with coordinates `x` and `y`.
 * @param ei1 First implicit ellipse parameters.
 * @param ei2 Second implicit ellipse parameters.
 * @param x X-coordinate.
 * @param y Y-coordinate.
 */
export const implicitIntersectionOfEllipsesFunc = (
  ei1: ImplicitEllipse,
  ei2: ImplicitEllipse,
  x: ad.Num,
  y: ad.Num
): ad.Num => {
  return max(implicitEllipseFunc(ei1, x, y), implicitEllipseFunc(ei2, x, y));
};

/**
 * Evaluate the implicit function for an half-plane at point with coordinates `x` and `y`.
 * @param hpi Implicit half-plane parameters.
 * @param x X-coordinate.
 * @param y Y-coordinate.
 */
export const implicitHalfPlaneFunc = (
  hpi: ImplicitHalfPlane,
  x: ad.Num,
  y: ad.Num
): ad.Num => {
  return sub(add(mul(hpi.a, x), mul(hpi.b, y)), hpi.c);
};

/**
 * Return implicit half-plane parameters given a line and a point inside the half-plane.
 * @param lineSegment Two points defining the line segment.
 * @param insidePoint Any point inside of the half-plane.
 * @param padding Padding around the Half-plane.
 */
export const halfPlaneToImplicit = (
  lineSegment: ad.Num[][],
  insidePoint: ad.Num[],
  padding: ad.Num
): ImplicitHalfPlane => {
  const normal = outwardUnitNormal(lineSegment, insidePoint);
  return {
    a: normal[0],
    b: normal[1],
    c: sub(ops.vdot(normal, lineSegment[0]), padding),
  };
};

/**
 * Return implicit ellipse parameters from an explicit representation.
 * @param ellipse Explicit ellipse shape.
 */
export const ellipseToImplicit = (
  ellipse: Ellipse,
  padding: ad.Num,
  factor: ad.Num = 1
): ImplicitEllipse => {
  const rx = add(ellipse.rx.contents, padding);
  const ry = add(ellipse.ry.contents, padding);
  return {
    a: mul(factor, div(ry, rx)),
    b: mul(factor, div(rx, ry)),
    c: mul(factor, mul(rx, ry)),
    x: ellipse.center.contents[0],
    y: ellipse.center.contents[1],
  };
};

/**
 * Return implicit ellipse parameters from an explicit circle.
 * @param circle Explicit circle shape.
 */
export const circleToImplicitEllipse = (
  circle: Circle,
  padding: ad.Num,
  factor: ad.Num = 1
): ImplicitEllipse => {
  return {
    a: factor,
    b: factor,
    c: mul(factor, squared(add(circle.r.contents, padding))),
    x: circle.center.contents[0],
    y: circle.center.contents[1],
  };
};

// Constant coefficient from the ellipse-ellipse polynomial
const ellipsePolynomialAlpha0 = (
  a: ImplicitEllipse,
  b: ImplicitEllipse
): ad.Num => {
  return mul(
    mul(squared(a.a), squared(a.b)),
    add(
      sub(a.c, b.c),
      add(mul(b.a, squared(sub(b.x, a.x))), mul(b.b, squared(sub(b.y, a.y))))
    )
  );
};

// Linear coefficient from the ellipse-ellipse polynomial
const ellipsePolynomialAlpha1 = (
  a: ImplicitEllipse,
  b: ImplicitEllipse
): ad.Num => {
  const coef = mul(2, mul(a.a, a.b));
  return mul(
    coef,
    add(
      mul(sub(a.c, b.c), sub(add(mul(a.a, b.b), mul(a.b, b.a)), coef)),
      add(
        mul(mul(a.a, b.a), mul(squared(sub(b.x, a.x)), sub(b.b, mul(2, a.b)))),
        mul(mul(a.b, b.b), mul(squared(sub(b.y, a.y)), sub(b.a, mul(2, a.a))))
      )
    )
  );
};

// Quadratic coefficient from the ellipse-ellipse polynomial
const ellipsePolynomialAlpha2 = (
  a: ImplicitEllipse,
  b: ImplicitEllipse
): ad.Num => {
  const coeff1 = add(
    add(
      mul(squared(a.a), squared(sub(b.b, a.b))),
      mul(squared(a.b), squared(sub(b.a, a.a)))
    ),
    mul(mul(4, mul(a.a, a.b)), mul(sub(b.b, a.b), sub(b.a, a.a)))
  );
  const coeff2 = sub(
    mul(squared(a.b), sub(mul(6, a.a), b.a)),
    mul(mul(a.a, b.b), sub(mul(6, a.b), b.b))
  );
  const coeff3 = sub(
    mul(squared(a.a), sub(mul(6, a.b), b.b)),
    mul(mul(a.b, b.a), sub(mul(6, a.a), b.a))
  );
  return add(
    mul(coeff1, sub(a.c, b.c)),
    add(
      mul(mul(coeff2, mul(a.a, b.a)), squared(sub(b.x, a.x))),
      mul(mul(coeff3, mul(a.b, b.b)), squared(sub(b.y, a.y)))
    )
  );
};

// Cubic coefficient from the ellipse-ellipse polynomial
const ellipsePolynomailAlpha3 = (
  a: ImplicitEllipse,
  b: ImplicitEllipse
): ad.Num => {
  const factor = mul(
    2,
    sub(mul(2, mul(a.a, a.b)), add(mul(a.b, b.a), mul(a.a, b.b)))
  );
  return mul(
    factor,
    add(
      mul(mul(sub(b.a, a.a), sub(b.b, a.b)), sub(b.c, a.c)),
      add(
        mul(mul(a.a, b.a), mul(squared(sub(b.x, a.x)), sub(b.b, a.b))),
        mul(mul(a.b, b.b), mul(squared(sub(b.y, a.y)), sub(b.a, a.a)))
      )
    )
  );
};

// Quintic coefficient from the ellipse-ellipse polynomial
const ellipsePolynomialAlpha4 = (
  a: ImplicitEllipse,
  b: ImplicitEllipse
): ad.Num => {
  return neg(
    add(
      mul(mul(squared(sub(b.a, a.a)), squared(sub(b.b, a.b))), sub(b.c, a.c)),
      add(
        mul(
          mul(mul(a.a, b.a), squared(sub(b.x, a.x))),
          mul(sub(b.a, a.a), squared(sub(b.b, a.b)))
        ),
        mul(
          mul(mul(a.b, b.b), squared(sub(b.y, a.y))),
          mul(squared(sub(b.a, a.a)), sub(b.b, a.b))
        )
      )
    )
  );
};

// Coefficients of the ellipse-ellipse polynomial
// (ordered from the lowest by their corresponding deggree)
const ellipsePolynomialParams = (
  a: ImplicitEllipse,
  b: ImplicitEllipse
): ad.Num[] => {
  return [
    ellipsePolynomialAlpha0(a, b),
    ellipsePolynomialAlpha1(a, b),
    ellipsePolynomialAlpha2(a, b),
    ellipsePolynomialAlpha3(a, b),
    ellipsePolynomialAlpha4(a, b),
  ];
};

// Return monic polynomial coefficients
// (the highest order coefficient is ommited and assumed to be 1)
export const ellipsePolynomial = (
  a: ImplicitEllipse,
  b: ImplicitEllipse
): ad.Num[] => {
  const params = ellipsePolynomialParams(a, b);
  // Prevent division by zero (note that `params[4]` being close 0
  // may still cause to instability in the root solver later)
  params[4] = ifCond(eq(params[4], 0), EPS_DENOM, params[4]);
  return Array.from(Array(4).keys()).map((i) => div(params[i], params[4]));
};
