import { mul, ops } from "@penrose/core";
import { DiagramBuilder } from "bloom/lib/core/builder.js";
import constraints from "bloom/lib/core/constraints.js";
import { Diagram } from "bloom/lib/core/diagram.js";
import objectives from "bloom/lib/core/objectives.js";
import { canvas } from "bloom/lib/core/utils.js";
import Renderer from "bloom/lib/react/Renderer.js";
import { useEffect, useState } from "react";

const graph = async () => {
  const {
    type,
    predicate,
    forall,
    forallWhere,
    circle,
    line,
    build,
    layer,
    encourage,
    ensure,
    input,
  } = new DiagramBuilder(canvas(400, 400), "");

  // domain
  const Node = type();
  const Edge = type();

  const Connects = predicate();
  const Optional = predicate();

  // substance
  const numNodes = 30;
  const numEdges = 20;

  const nodes = [];
  for (let i = 0; i < numNodes; ++i) {
    const n = Node();
    nodes.push(n);
  }

  for (let i = 0; i < numEdges; ++i) {
    const e = Edge();
    let a, b;
    do {
      const j = Math.floor(Math.random() * numNodes);
      const k = Math.floor(Math.random() * numNodes);
      if (k === j) continue;
      a = nodes[j];
      b = nodes[k];
    } while (Connects.test(e, a, b) || Connects.test(e, b, a));

    Connects(e, a, b);
    if (Math.random() < 0.5) {
      Optional(e);
    }
  }

  // style
  forall({ a: Node }, ({ a }) => {
    a.icon = circle({
      r: 5,
      fillColor: [0, 0, 0, 1],
      drag: true,
    });
  });

  const optionalOpacity = input({
    name: "optionalOpacity",
    init: 1,
    pinned: true,
  });
  forallWhere(
    { e: Edge, a: Node, b: Node },
    ({ e, a, b }) => Connects.test(e, a, b),
    ({ e, a, b }) => {
      e.opacity = Optional.test(e) ? optionalOpacity : 1;

      e.icon = line({
        start: a.icon.center,
        end: b.icon.center,
        strokeColor: [0, 0, 0, e.opacity],
        strokeWidth: 2,
        strokeStyle: Optional.test(e) ? "dashed" : "solid",
      });

      const length = ops.vnorm(ops.vsub(a.icon.center, b.icon.center));
      ensure(mul(e.opacity, constraints.equal(length, 100)));

      layer(e.icon, a.icon);
      layer(e.icon, b.icon);
    },
  );

  // forall nodes, repel each other
  forall({ a: Node, b: Node }, ({ a, b }) => {
    encourage(objectives.repelPt(1, a.icon.center, b.icon.center));
  });

  return await build();
};

export default function GraphComponent() {
  const [diagram, setDiagram] = useState<Diagram | null>(null);

  useEffect(() => {
    graph().then(setDiagram);
  }, []);

  if (!diagram) {
    return <div>Loading...</div>;
  }

  // create a renderer component rendering the diagram and a button to toggle the optional edges
  return (
    <div
      style={{
        height: "90%",
      }}
    >
      <button
        onClick={() => {
          const optionalOpacity = diagram.getInput("optionalOpacity");
          diagram.setInput("optionalOpacity", 1 - optionalOpacity);
        }}
      >
        Toggle Optional Edges
      </button>
      <Renderer diagram={diagram} />
    </div>
  );
}
