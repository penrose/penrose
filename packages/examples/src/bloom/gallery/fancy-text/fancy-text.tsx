/** @jsxImportSource @penrose/bloom */
// Gallery translation of: fancy-text/fancy-text

import seedrandom from "seedrandom";

import type { Diagram } from "@penrose/bloom";
import {
  DiagramBuilder,
  canvas,
  constraints,
  objectives,
} from "@penrose/bloom";

import { rgba } from "../utils.js";

const SEED = "ValentinoDonkey58541";
const labels = [
  "\\alpha",
  "\\beta",
  "\\gamma",
  "\\delta",
  "\\epsilon",
  "\\zeta",
  "\\eta",
  "\\theta",
  "\\iota",
  "\\kappa",
  "\\lambda",
  "\\mu",
  "\\nu",
  "\\xi",
  "\\omicron",
  "\\pi",
  "\\rho",
  "\\sigma",
  "\\tau",
  "\\upsilon",
  "\\phi",
  "\\chi",
  "\\psi",
  "\\omega",
];

const randomUnit = (rng: seedrandom.prng): [number, number, number] => {
  const z = rng() * 2 - 1;
  const theta = rng() * Math.PI * 2;
  const r = Math.sqrt(1 - z * z);
  return [r * Math.cos(theta), r * Math.sin(theta), z];
};

export const buildDiagram = async (variation = SEED): Promise<Diagram> => {
  const { build, ensure, encourage, layer, type, forall } = new DiagramBuilder(
    canvas(533, 300),
    variation,
  );

  const TextBlock = type();
  const Point = type();

  const title = TextBlock();
  title.label = "Hello, Penrose!";

  const points = labels.map((label) => {
    const point = Point();
    point.label = label;
    return point;
  });

  const rng = seedrandom(variation);
  const fillData = points.map(() => {
    const [x, y, z] = randomUnit(rng);
    const radius = 18 + rng() * 20;
    return {
      radius,
      circleFill: rgba(
        Math.abs(x * 127.5),
        Math.abs(y * 127.5),
        Math.abs(z * 255),
        0.25,
      ),
      textFill: rgba(
        Math.abs(x * 63.75),
        Math.abs(y * 63.75),
        Math.abs(z * 127.5),
        0.25,
      ),
    };
  });

  const bbox = (
    <rect
      center={[0, 0]}
      width={533}
      height={300}
      fill-color={rgba(204, 242, 230, 1)}
      stroke-color={rgba(153, 217, 153, 1)}
      stroke-width={4}
    />
  );

  forall({ t: TextBlock }, ({ t }) => {
    t.outerStroke = (
      <text
        string={t.label}
        center={[0, 0]}
        fill-color={rgba(255, 102, 3, 1)}
        font-family={"Palatino"}
        font-style={"italic"}
        font-size={"38px"}
        stroke-width={8}
        stroke-color={rgba(255, 102, 3, 1)}
        paint-order={"stroke"}
      />
    );
    t.text = (
      <text
        string={t.label}
        center={[0, 0]}
        fill-color={rgba(255, 255, 255, 1)}
        font-family={"Palatino"}
        font-style={"italic"}
        font-size={"38px"}
        stroke-width={4}
        stroke-color={rgba(255, 3, 3, 1)}
        paint-order={"stroke"}
      />
    );
    layer(bbox, t.outerStroke);
    layer(t.outerStroke, t.text);
  });

  points.forEach((point, index) => {
    const { radius, circleFill, textFill } = fillData[index];
    point.radius = radius;
    point.circleFill = circleFill;
    point.textFill = textFill;
  });

  forall({ p: Point }, ({ p }) => {
    p.icon = <circle r={p.radius} fill-color={p.circleFill} />;
    p.text = (
      <equation
        center={p.icon.center}
        string={p.label}
        fill-color={p.textFill}
        font-size={"28px"}
      />
    );

    ensure(constraints.contains(bbox, p.icon));
    layer(bbox, p.icon);
    layer(bbox, p.text);
  });

  forall({ p: Point, q: Point }, ({ p, q }) => {
    encourage(objectives.repelPt(200000, p.icon.center, q.icon.center));
  });

  return await build();
};
