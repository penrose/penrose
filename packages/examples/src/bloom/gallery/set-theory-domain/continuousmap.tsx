/** @jsxImportSource @penrose/bloom */
// Gallery translation of: set-theory-domain/continuousmap

import type { Diagram } from "@penrose/bloom";
import {
  DiagramBuilder,
  add,
  canvas,
  constraints,
  objectives,
  sub,
} from "@penrose/bloom";

import { rgba } from "../utils.js";

const SEED = "ContinuousMapBloom";

export const buildDiagram = async (variation = SEED): Promise<Diagram> => {
  const { ensure, encourage, build, layer, type, predicate } =
    new DiagramBuilder(canvas(800, 700), variation);

  const Set = type();
  const Map = type();
  const Subset = predicate();
  const From = predicate();

  const strokeWidth = 1.5;
  const padding = 20;
  const iconSize = 700 / 3;
  const cornerOffset = iconSize / 2 - padding;

  const A = Set();
  const U = Set();
  const Rn = Set();
  const B = Set();
  const V = Set();
  const Rm = Set();
  const f = Map();

  A.label = "A";
  U.label = "f^{-1}(V)";
  Rn.label = "\\mathbb{R}^n";
  B.label = "B";
  V.label = "V";
  Rm.label = "\\mathbb{R}^m";
  f.label = "f";

  Subset(U, A);
  Subset(A, Rn);
  Subset(V, B);
  Subset(B, Rm);
  From(f, A, B);

  Rn.icon = (
    <rect
      width={iconSize}
      height={iconSize}
      fill-color={rgba(242, 245, 235, 0.5)}
      stroke-width={strokeWidth}
      stroke-color={rgba(0, 0, 0, 1)}
    />
  );
  Rn.text = (
    <equation
      string={Rn.label}
      center={[
        add(Rn.icon.center[0], cornerOffset),
        add(Rn.icon.center[1], cornerOffset),
      ]}
    />
  );
  layer(Rn.icon, Rn.text);

  Rm.icon = (
    <rect
      center={[add(Rn.icon.center[0], 400), Rn.icon.center[1]]}
      width={iconSize}
      height={iconSize}
      fill-color={rgba(242, 245, 235, 0.5)}
      stroke-width={strokeWidth}
      stroke-color={rgba(0, 0, 0, 1)}
    />
  );
  Rm.text = (
    <equation
      string={Rm.label}
      center={[
        add(Rm.icon.center[0], cornerOffset),
        add(Rm.icon.center[1], cornerOffset),
      ]}
    />
  );
  layer(Rm.icon, Rm.text);

  for (const x of [A, U, B, V]) {
    const dashed = x === U || x === V;
    x.icon = (
      <circle
        fill-color={rgba(26, 26, 230, 0.2)}
        stroke-color={rgba(0, 0, 0, 1)}
        stroke-width={dashed ? strokeWidth : 1}
        stroke-style={dashed ? "dashed" : "solid"}
      />
    );
    x.text = <equation string={x.label} />;
    ensure(constraints.contains(x.icon, x.text));
    layer(x.icon, x.text);
  }

  for (const [small, large] of [
    [U, A],
    [A, Rn],
    [V, B],
    [B, Rm],
  ] as const) {
    ensure(constraints.contains(large.icon, small.icon, 10));
    ensure(constraints.disjoint(large.text, small.icon, 1));
    layer(large.icon, small.icon);
  }

  f.icon = (
    <line
      start={[
        add(Rn.icon.center[0], iconSize / 2 + padding),
        Rn.icon.center[1],
      ]}
      end={[sub(Rm.icon.center[0], iconSize / 2 + padding), Rm.icon.center[1]]}
      stroke-width={2}
      stroke-color={rgba(0, 0, 0, 1)}
      end-arrowhead={"straight"}
    />
  );
  f.text = <equation string={f.label} />;
  encourage(objectives.centerLabelAbove(f.icon, f.text, 5));

  return await build();
};
