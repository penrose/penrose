/** @jsxImportSource @penrose/bloom */

/**
 * A diagram demonstrating JSX functional components in Bloom.
 * Nodes are rendered as circles with a reusable `NodeShape`
 * functional component. Demonstrates kebab-case prop names.
 */
import type { Diagram } from "@penrose/bloom";
import { canvas, DiagramBuilder, objectives } from "@penrose/bloom";

/** Reusable functional component: a styled circle node */
const NodeShape = () => (
  <circle
    r={50}
    fill-color={[0.3, 0.6, 0.9, 0.9]}
    stroke-color={[0.1, 0.3, 0.6, 1]}
    stroke-width={2}
    ensure-on-canvas
  />
);

export const buildCustomComponentsDiagram = async (): Promise<Diagram> => {
  const db = new DiagramBuilder(canvas(500, 400), "custom-components");
  const { type, predicate, forall, encourage, build } = db;

  const Node = type();
  const Connected = predicate();

  // Create a small graph: a — b — c, a — c
  const a = Node();
  const b = Node();
  const c = Node();

  Connected(a, b);
  Connected(b, c);
  Connected(a, c);

  // Draw each node using the functional NodeShape component
  forall({ n: Node }, ({ n }) => {
    n.icon = <NodeShape />;
  });

  // Draw edges between connected nodes
  forall({ u: Node, v: Node }, ({ u, v }) => {
    if (Connected.test(u, v)) {
      u.edgeTo = (
        <line
          start={u.icon.center}
          end={v.icon.center}
          stroke-color={[0.4, 0.4, 0.4, 0.8]}
          stroke-width={1.5}
        />
      );
    }
  });

  // Repel nodes from each other
  forall({ u: Node, v: Node }, ({ u, v }) => {
    encourage(objectives.notTooClose(u.icon, v.icon), 2);
  });

  return await build();
};

export { buildCustomComponentsDiagram as buildDiagram };

export default async (): Promise<string> => {
  const diagram = await buildCustomComponentsDiagram();
  const { svg } = await diagram.render();
  return svg.outerHTML;
};
