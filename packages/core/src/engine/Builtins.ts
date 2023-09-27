import {
  Bool,
  Dual,
  Real,
  add,
  addLin,
  div,
  divLin,
  fn,
  gt,
  lt,
  mul,
  mulLin,
  neg,
  opaque,
  sqrt as roseSqrt,
  select,
  sub,
  subLin,
} from "rose";

export const mask = fn([Real, Bool], Real, (x) => x);
mask.jvp = fn([Dual, Bool], Dual, (z, p) =>
  select(p, Dual, z, { re: z.re, du: 0 }),
);

export const epsilon = 1e-5;

export const max = (x: Real, y: Real) => select(gt(x, y), Real, x, y);
export const min = (x: Real, y: Real) => select(lt(x, y), Real, x, y);

export const sqrt = fn([Real], Real, (x) => roseSqrt(x));
sqrt.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = sqrt(x);
  // NOTE: Watch out for divide by zero in 1 / [2 sqrt(x)]
  const dy = mulLin(dx, div(1 / 2, max(epsilon, y)));
  return { re: y, du: dy };
});

export const acos = opaque([Real], Real, Math.acos);
export const acosh = opaque([Real], Real, Math.acosh);
export const asin = opaque([Real], Real, Math.asin);
export const asinh = opaque([Real], Real, Math.asinh);
export const atan = opaque([Real], Real, Math.atan);
export const atanh = opaque([Real], Real, Math.atanh);
export const cbrt = opaque([Real], Real, Math.cbrt);
export const cos = opaque([Real], Real, Math.cos);
export const cosh = opaque([Real], Real, Math.cosh);
export const exp = opaque([Real], Real, Math.exp);
export const expm1 = opaque([Real], Real, Math.expm1);
export const log = opaque([Real], Real, Math.log);
export const log10 = opaque([Real], Real, Math.log10);
export const log1p = opaque([Real], Real, Math.log1p);
export const log2 = opaque([Real], Real, Math.log2);
export const sin = opaque([Real], Real, Math.sin);
export const sinh = opaque([Real], Real, Math.sinh);
export const tan = opaque([Real], Real, Math.tan);
export const tanh = opaque([Real], Real, Math.tanh);

export const atan2 = opaque([Real, Real], Real, Math.atan2);
export const pow = opaque([Real, Real], Real, Math.pow);

acos.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = acos(x);
  const dy = divLin(dx, neg(sqrt(sub(1, mul(x, x)))));
  return { re: y, du: dy };
});

acosh.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = acosh(x);
  const dy = divLin(dx, mul(sqrt(sub(x, 1)), sqrt(add(x, 1))));
  return { re: y, du: dy };
});

asin.jvp = fn([Dual, Dual], Dual, ({ re: x, du: dx }) => {
  const y = asin(x);
  const dy = divLin(dx, sqrt(sub(1, mul(x, x))));
  return { re: y, du: dy };
});

asinh.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = asinh(x);
  const dy = divLin(dx, sqrt(add(1, mul(x, x))));
  return { re: y, du: dy };
});

atan.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = atan(x);
  const dy = divLin(dx, add(1, mul(x, x)));
  return { re: y, du: dy };
});

atanh.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = atanh(x);
  const dy = divLin(dx, sub(1, mul(x, x)));
  return { re: y, du: dy };
});

cbrt.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = cbrt(x);
  const dy = mulLin(dx, div(1 / 3, mul(y, y)));
  return { re: y, du: dy };
});

cos.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = cos(x);
  const dy = mulLin(dx, neg(sin(x)));
  return { re: y, du: dy };
});

cosh.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = cosh(x);
  const dy = mulLin(dx, sinh(x));
  return { re: y, du: dy };
});

exp.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = exp(x);
  const dy = mulLin(dx, y);
  return { re: y, du: dy };
});

expm1.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = expm1(x);
  const dy = mulLin(dx, add(y, 1));
  return { re: y, du: dy };
});

log.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = log(x);
  const dy = divLin(dx, x);
  return { re: y, du: dy };
});

log10.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = log10(x);
  const dy = mulLin(dx, div(Math.LOG10E, x));
  return { re: y, du: dy };
});

log1p.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = log1p(x);
  const dy = divLin(dx, add(1, x));
  return { re: y, du: dy };
});

log2.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = log2(x);
  const dy = mulLin(dx, div(Math.LOG2E, x));
  return { re: y, du: dy };
});

sin.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = sin(x);
  const dy = mulLin(dx, cos(x));
  return { re: y, du: dy };
});

sinh.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = sinh(x);
  const dy = mulLin(dx, cosh(x));
  return { re: y, du: dy };
});

tan.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = tan(x);
  const dy = mulLin(dx, add(1, mul(y, y)));
  return { re: y, du: dy };
});

tanh.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  const y = tanh(x);
  const dy = mulLin(dx, sub(1, mul(y, y)));
  return { re: y, du: dy };
});

atan2.jvp = fn([Dual, Dual], Dual, ({ re: y, du: dy }, { re: x, du: dx }) => {
  const z = atan2(y, x);
  const dz = divLin(
    subLin(mulLin(dy, x), mulLin(dx, y)),
    add(mul(x, x), mul(y, y)),
  );
  return { re: z, du: dz };
});

pow.jvp = fn([Dual, Dual], Dual, ({ re: x, du: dx }, { re: y, du: dy }) => {
  const z = pow(x, y);
  const dz = mulLin(addLin(mulLin(dx, div(y, x)), mulLin(dy, log(x))), z);
  return { re: z, du: dz };
});
