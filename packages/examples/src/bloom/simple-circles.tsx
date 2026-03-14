/** @jsxImportSource @penrose/bloom */

/**
 * A simple diagram demonstrating Bloom JSX syntax:
 * three labeled circles that don't overlap and stay on canvas.
 */
import { canvas, DiagramBuilder, objectives } from "@penrose/bloom";

export default async (): Promise<string> => {
  const db = new DiagramBuilder(canvas(400, 400), "bloom-circles");
  const { type, forall, ensure, encourage, build } = db;

  const Node = type();

  // Create three nodes
  Node();
  Node();
  Node();

  forall({ n: Node }, ({ n }) => {
    n.icon = (
      <circle
        r={40}
        fill-color={[0.2, 0.4, 0.8, 0.8]}
        stroke-color={[0.1, 0.2, 0.5, 1]}
        stroke-width={2}
        ensure-on-canvas
      />
    );
  });

  // Keep circles apart from each other
  forall({ a: Node, b: Node }, ({ a, b }) => {
    encourage(objectives.notTooClose(a.icon, b.icon));
  });

  const diagram = await build();
  const { svg } = await diagram.render();
  return svg.outerHTML;
};
