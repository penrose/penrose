/** @jsxImportSource @penrose/bloom */
import {
  canvas,
  constraints,
  DiagramBuilder,
  objectives,
  ops,
  type Circle,
  type RawSvgElement,
} from "@penrose/bloom";

import tigerSvg from "./tiger.svg.js";

const SEED = "tiger-eyes";
const CANVAS_SIZE = 900;

// Tiger eye positions in Bloom canvas coordinates (origin at center, y-up).
// Derived from the SVG's pre-transform coords using:
//   transform="matrix(1.7656463,0,0,1.7656463,324.90716,255.00942)"
// Screen coords → Bloom:  bx = sx − 450,  by = 450 − sy
//
// Right iris  (g302 #99cc32): SVG screen ≈ (408, 297) → Bloom (−42, 153)
// Left iris   (g350 #99cc32): SVG screen ≈ (201, 262) → Bloom (−249, 188)
const RIGHT_IRIS: [number, number] = [-42, 153];
const LEFT_IRIS: [number, number] = [-249, 188];
const RIGHT_IRIS_R = 15;
const LEFT_IRIS_R = 10;

/** Recursively convert a DOM Element to a Bloom RawSvgElement tree. */
const elementToRaw = (el: Element): RawSvgElement => {
  const attrs: Record<string, string> = {};
  for (const { name, value } of Array.from(el.attributes)) {
    attrs[name] = value;
  }
  return {
    _rawSvg: true,
    tag: el.tagName,
    attrs,
    children: Array.from(el.children).map(elementToRaw),
  };
};

export const buildDiagram = async (variation = SEED) => {
  const db = new DiagramBuilder(canvas(CANVAS_SIZE, CANVAS_SIZE), variation);
  const { ensure, encourage, build } = db;

  // Inject the tiger body as a raw SVG background.
  // We extract the inner <g> group (not the <svg> root) so it renders
  // directly in the Bloom SVG's coordinate space.
  const parser = new DOMParser();
  const doc = parser.parseFromString(tigerSvg, "image/svg+xml");
  const tigerGroup = doc.documentElement.children[0]; // <g id="g4" transform="...">
  db.addRawSvgDef(elementToRaw(tigerGroup));

  // Named inputs updated externally for interactive cursor-following eyes.
  const cursorX = db.input({ name: "cursorX", init: 0, optimized: false });
  const cursorY = db.input({ name: "cursorY", init: 0, optimized: false });

  // Right pupil — attracted to cursor, constrained within the right iris.
  const rightPupil = (
    <circle r={4} fill-color={[0.05, 0.05, 0.05, 1]} ensure-on-canvas={false} />
  ) as unknown as Circle;
  ensure(
    constraints.lessThan(
      ops.vnorm(ops.vsub(rightPupil.center, RIGHT_IRIS)),
      RIGHT_IRIS_R - 5,
    ),
  );
  encourage(objectives.equal(rightPupil.center[0], cursorX));
  encourage(objectives.equal(rightPupil.center[1], cursorY));

  // Left pupil — attracted to cursor, constrained within the left iris.
  const leftPupil = (
    <circle r={3} fill-color={[0.05, 0.05, 0.05, 1]} ensure-on-canvas={false} />
  ) as unknown as Circle;
  ensure(
    constraints.lessThan(
      ops.vnorm(ops.vsub(leftPupil.center, LEFT_IRIS)),
      LEFT_IRIS_R - 4,
    ),
  );
  encourage(objectives.equal(leftPupil.center[0], cursorX));
  encourage(objectives.equal(leftPupil.center[1], cursorY));

  return build();
};
