import { DiagramBuilder } from "bloom/lib/core/builder.js";
import { canvas } from "bloom/lib/core/utils.js";
import { useDiagram } from "../../lib";
import Renderer from "../../lib/react/Renderer.tsx";

const raysDiagram = async () => {
  const { rectangle, circle, group, build, input, layer } = new DiagramBuilder(
    canvas(400, 400),
    "",
  );

  const r1 = rectangle({
    width: 100,
    height: 20,
    center: [
      input({ init: 0, pinned: true }),
      input({ init: 0, pinned: true }),
    ],
    fillColor: [1, 0, 0, 0.5],
    drag: true,
  });

  const r2 = rectangle({
    width: 100,
    height: 20,
    center: [
      input({ init: 0, pinned: true }),
      input({ init: 10, pinned: true }),
    ],
    fillColor: [0, 1, 0, 1],
    drag: true,
  });

  // layer(r1, r2);

  const c = circle({
    r: 50,
    center: [0, 0],
  });

  const g = group({
    shapes: [r2, r1],
    clipPath: c,
  });

  return await build();
};

export default function RaysComponent() {
  const diagram = useDiagram(raysDiagram);

  if (!diagram) return <div>Loading...</div>;
  else return <Renderer diagram={diagram} />;
}
