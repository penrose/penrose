import { ops } from "@penrose/core";
import { DiagramBuilder } from "bloom/lib/core/builder.js";
import { canvas } from "bloom/lib/core/utils.js";
import { Shape, useDiagram } from "../../lib";
import computation from "../../lib/core/computation.ts";
import Renderer from "../../lib/react/Renderer.tsx";

const raysDiagram = async () => {
  const {
    rectangle,
    circle,
    group,
    build,
    input,
    layer,
    type,
    forall,
    line,
    polygon,
  } = new DiagramBuilder(canvas(400, 400), "");

  const Ray = type();

  const Square = type();
  const Triangle = type();
  const Source = type();
  const Ground = type();

  const numRays = 100;
  for (let i = 0; i < numRays; ++i) {
    Ray();
  }

  Triangle();
  Square();

  const source = Source();
  const ground = Ground();

  forall({ g: Ground }, ({ g }) => {
    g.level = -175;
    g.icon = line({
      start: [-200, g.level],
      end: [200, g.level],
      strokeWidth: 3,
      strokeColor: [0, 0, 0, 0.5],
    });
  });

  forall({ s: Source }, ({ s }) => {
    s.icon = circle({
      center: [0, 175],
      r: 3,
      fillColor: [0, 0, 0, 1],
    });
  });

  const shapes: Shape[] = [];

  forall({ t: Triangle }, ({ t }) => {
    t.icon = polygon({
      points: [
        [input(), input()],
        [input(), input()],
        [input(), input()],
      ],
    });

    shapes.push(t.icon);
  });

  forall({ s: Square }, ({ s }) => {
    s.icon = rectangle({
      drag: true,
    });

    shapes.push(s.icon);
  });

  group({
    shapes,
  });

  forall({ r: Ray }, ({ r }, i) => {
    const extendedToGround = [
      ground.icon.start[0] +
        ((ground.icon.end[0] - ground.icon.start[0]) * i) / (numRays - 1),
      ground.level,
    ];
    r.vec = computation.normalize(
      ops.vsub(extendedToGround, source.icon.center),
    );
  });

  return await build();
};

export default function RaysComponent() {
  const diagram = useDiagram(raysDiagram);

  if (!diagram) return <div>Loading...</div>;
  else return <Renderer diagram={diagram} />;
}
