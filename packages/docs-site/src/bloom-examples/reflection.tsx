import {
  DiagramBuilder,
  canvas,
  constraints,
  useDiagram,
} from "@penrose/bloom";
import Renderer from "@penrose/bloom/dist/react/Renderer.js";
import { mul, ops } from "@penrose/core";

export const reflection = async () => {
  const db = new DiagramBuilder(canvas(800, 400), "");
  const { type, line, circle, build, input, ensure, forall } = db;

  const Endpoint = type();
  const Ray = type();
  const Mirror = type();

  const start = Endpoint();
  const end = Endpoint();
  const ray1 = Ray();
  const ray2 = Ray();
  const mirror = Mirror();

  // draw mirror
  mirror.level = -150;
  mirror.icon = line({
    start: [-400, mirror.level],
    end: [400, mirror.level],
    strokeWidth: 5,
    strokeColor: [0, 0, 0, 0.5],
  });

  // Draw endpoints
  const handleRad = 40;

  start.center = [
    input({ init: -300, optimized: false }),
    input({ init: 100, optimized: false }),
  ];
  end.center = [
    input({ init: 300, optimized: false }),
    input({ init: 100, optimized: false }),
  ];

  forall({ e: Endpoint }, ({ e }) => {
    e.dot = circle({
      center: e.center,
      r: 5,
      fillColor: [0, 0, 0, 1],
    });
    e.handle = circle({
      center: e.center,
      r: handleRad,
      drag: true,
      fillColor: [0, 0, 0, 0.1],
    });
  });

  // draw rays
  const reflectPoint = [input({ init: 0 }), mirror.level];

  ray1.start = start.center;
  ray1.end = reflectPoint;

  ray2.start = reflectPoint;
  ray2.end = end.center;

  forall({ r: Ray }, ({ r }) => {
    r.icon = line({
      start: r.start,
      end: r.end,
      endArrowhead: "straight",
      // purple
      strokeColor: [0.5, 0, 0.5, 1],
      strokeWidth: 3,
    });
    r.normVec = ops.vnormalize(ops.vsub(r.end, r.start));
  });

  const r1y = ray1.normVec[1];
  const r2y = ray2.normVec[1];
  ensure(constraints.equal(r1y, mul(-1, r2y)));

  const r1x = ray1.normVec[0];
  const r2x = ray2.normVec[0];
  ensure(constraints.equal(r1x, mul(1, r2x)));

  return await build();
};

export default function ReflectionDiagram() {
  const reflectionDiagram = useDiagram(reflection);

  if (!reflectionDiagram) {
    return <div>Loading...</div>;
  }
  return (
    <div
      style={{
        marginTop: "2em",
        height: "20em",
      }}
    >
      <Renderer diagram={reflectionDiagram} />
    </div>
  );
}
