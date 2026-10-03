/** @jsxImportSource @penrose/bloom */
// Gallery translation of: set-theory-domain/tree-euler-3d

import type { Diagram } from "@penrose/bloom";
import {
  DiagramBuilder,
  canvas,
  constraints,
  objectives,
  ops,
  rgba,
} from "@penrose/bloom";
import { mul } from "@penrose/core";

export const buildDiagram = async (): Promise<Diagram> => {
  const { ensure, encourage, build, type, predicate, forall, layer } =
    new DiagramBuilder(canvas(800, 700), "BourbonFalcon9173");

  // domain
  const Set = type();
  const IsSubset = predicate();
  const IsDisjoint = predicate();

  // substance
  const A = Set();
  const B = Set();
  const C = Set();
  const D = Set();
  const E = Set();
  const F = Set();
  const G = Set();

  A.label = "A";
  B.label = "B";
  C.label = "C";
  D.label = "D";
  E.label = "E";
  F.label = "F";
  G.label = "G";

  IsSubset(A, B);
  IsSubset(A, C);

  IsSubset(B, D);
  IsSubset(B, E);

  IsSubset(C, F);
  IsSubset(C, G);

  IsDisjoint(B, C);
  IsDisjoint(D, E);
  IsDisjoint(F, G);

  <defs>
    <radialGradient
      id="radial-gradient"
      gradientUnits="objectBoundingBox"
      cx="0.4687"
      cy="0.4569"
      fx="0.2739"
      fy="0.2402"
      r="0.689"
    >
      <stop offset="0.14" stop-color="#dbdbdb" />
      <stop offset="0.29" stop-color="#bdbdbd" />
      <stop offset="0.6" stop-color="#707070" />
      <stop offset="0.72" stop-color="#525252" />
      <stop offset="0.76" stop-color="#656565" />
      <stop offset="0.84" stop-color="#959595" />
      <stop offset="0.96" stop-color="#e2e2e2" />
      <stop offset="1" stop-color="#fff" />
    </radialGradient>
    <linearGradient
      id="linear-gradient"
      x1="214.17"
      y1="30.72"
      x2="996.08"
      y2="1361.33"
      gradientUnits="userSpaceOnUse"
    >
      <stop offset="0.14" stop-color="#dbdbdb" stop-opacity="0" />
      <stop offset="0.88" />
    </linearGradient>
  </defs>;

  forall({ x: Set }, ({ x }) => {
    // Main shape
    x.icon = <circle stroke-width={2} drag={true} ensure-on-canvas />;

    x.shading = (
      <circle
        center={x.icon.center}
        r={x.icon.r}
        fill="url(#radial-gradient)"
      />
    );
    x.shadow = (
      <ellipse
        center={ops.vadd(x.icon.center, [30, -10])}
        rx={mul(x.icon.r, 1.02)}
        ry={mul(x.icon.r, 1.07)}
        opacity="0.3"
        fill="url(#linear-gradient)"
      />
    );
    layer(x.shadow, x.icon);
    layer(x.shading, x.icon);

    // Label
    x.text = (
      <equation
        string={x.label}
        width={mul(x.icon.r, 0.4)}
        height={mul(x.icon.r, 0.4)}
        fillColor={rgba(1, 1, 1, 1)}
        ensure-on-canvas
      />
    );
    layer(x.icon, x.text);

    ensure(constraints.greaterThan(x.icon.r, 20));
    ensure(constraints.contains(x.icon, x.text));
    encourage(objectives.sameCenter(x.text, x.icon));
  });

  forall({ a: Set, b: Set }, ({ a, b }) => {
    if (IsSubset.test(a, b)) {
      ensure(constraints.contains(a.icon, b.icon, 10));
      ensure(constraints.disjoint(a.text, b.icon, 5));
      layer(a.icon, b.icon);
    }
  });

  forall({ a: Set, b: Set }, ({ a, b }) => {
    if (IsDisjoint.test(a, b)) {
      ensure(constraints.disjoint(a.icon, b.icon, 10));
    }
  });

  return await build();
};
