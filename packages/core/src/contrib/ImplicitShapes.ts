import { outwardUnitNormal } from "contrib/Queries";
import { ops } from "engine/Autodiff";
import { add, div, mul, neg, squared, sub } from "engine/AutodiffFunctions";
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
export const ellipseToImplicit = (ellipse: Ellipse): ImplicitEllipse => {
  const rx = ellipse.rx.contents;
  const ry = ellipse.ry.contents;
  return {
    a: div(ry, rx),
    b: div(rx, ry),
    c: mul(rx, ry),
    x: ellipse.center.contents[0],
    y: ellipse.center.contents[1],
  };
};

// Constant coefficient from the ellipse-ellipse polynomial
const ellipsePolynomailAlpha0 = (
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
const ellipsePolynomailAlpha1 = (
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
const ellipsePolynomailAlpha2 = (
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
  b: ImplicitEllipse,
  beta: ad.Num
): ad.Num => {
  return mul(
    sub(mul(2, mul(a.a, a.b)), add(mul(a.b, b.a), mul(a.a, b.b))),
    mul(mul(2, beta), mul(sub(b.a, a.a), sub(b.b, a.b)))
  );
};

// Quintic coefficient from the ellipse-ellipse polynomial
const ellipsePolynomailAlpha4 = (
  a: ImplicitEllipse,
  b: ImplicitEllipse,
  beta: ad.Num
): ad.Num => {
  return mul(neg(beta), mul(squared(sub(b.a, a.a)), squared(sub(b.b, a.b))));
};

// Helper function for the ellipse-ellipse polynomial
const ellipsePolynomailBeta = (
  a: ImplicitEllipse,
  b: ImplicitEllipse
): ad.Num => {
  return add(
    sub(b.c, a.c),
    add(
      div(mul(mul(a.a, b.a), squared(sub(b.x, a.x))), sub(b.a, a.a)),
      div(mul(mul(a.b, b.b), squared(sub(b.y, a.y))), sub(b.b, a.b))
    )
  );
};

export const ellipsePolynomial = (
  a: ImplicitEllipse,
  b: ImplicitEllipse
): ad.Num[] => {
  const beta = ellipsePolynomailBeta(a, b);
  return [
    ellipsePolynomailAlpha0(a, b),
    ellipsePolynomailAlpha1(a, b),
    ellipsePolynomailAlpha2(a, b),
    ellipsePolynomailAlpha3(a, b, beta),
    ellipsePolynomailAlpha4(a, b, beta),
  ];
};
