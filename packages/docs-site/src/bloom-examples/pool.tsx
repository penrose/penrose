import {
  DiagramBuilder,
  Renderer,
  canvas,
  constraints,
  rayIntersectRect,
  useDiagram,
} from "@penrose/bloom";
import { ops } from "@penrose/core";

const poolDiagram = async () => {
  const db = new DiagramBuilder(canvas(800, 700), "", 1000);
  const {
    type,
    predicate,
    line,
    circle,
    build,
    input,
    ensure,
    forall,
    rectangle,
    forallWhere,
    group,
  } = db;

  const Border = type();
  const Pocket = type();
  const Queue = type();
  const Ray = type();

  const border = Border();

  const CornerPocket = predicate();
  const SidePocket = predicate();

  // clockwise from top left
  const pockets = Array.from({ length: 6 }, () => Pocket());

  CornerPocket(pockets[0]);
  SidePocket(pockets[1]);
  CornerPocket(pockets[2]);
  CornerPocket(pockets[3]);
  SidePocket(pockets[4]);
  CornerPocket(pockets[5]);

  const queue = Queue();
  const ray = Ray();

  const tableWidth = 600;
  const tableHeight = 300;

  border.icon = rectangle({
    width: tableWidth,
    height: tableHeight,
    fillColor: [0, 0, 0, 0],
    strokeWidth: 3,
    strokeColor: [0, 0, 0, 1],
    center: [0, 0],
  });

  forallWhere(
    { p: Pocket },
    ({ p }) => CornerPocket.test(p),
    ({ p }, i) => {
      p.pocket = circle({
        center: [
          i % 2 === 0 ? -tableWidth / 2 : tableWidth / 2,
          i < 2 ? tableHeight / 2 : -tableHeight / 2,
        ],
        r: 20,
        fillColor: [1, 1, 1, 1],
        strokeWidth: 3,
        strokeColor: [0, 0, 0, 1],
      });
    },
  );

  forallWhere(
    { p: Pocket },
    ({ p }) => SidePocket.test(p),
    ({ p }, i) => {
      p.pocket = circle({
        center: [0, i % 2 === 0 ? tableHeight / 2 : -tableHeight / 2],
        r: 20,
        fillColor: [1, 1, 1, 1],
        strokeWidth: 3,
        strokeColor: [0, 0, 0, 1],
      });
    },
  );

  border.mask = rectangle({
    width: tableWidth - 3,
    height: tableHeight - 3,
    fillColor: [1, 1, 1, 1],
    center: [0, 0],
  });

  const queueLength = 200;
  queue.startHandle = circle({
    center: [input({ init: 0 }), input({ init: 0 })],
    r: 20,
    fillColor: [0, 0, 0, 0.1],
    drag: true,
  });
  queue.endHandle = circle({
    center: [input({ init: 0 }), input({ init: -200 })],
    r: 20,
    fillColor: [0, 0, 0, 0.1],
    drag: true,
  });
  queue.icon = line({
    start: queue.startHandle.center,
    end: queue.endHandle.center,
    strokeWidth: 3,
    strokeColor: [0, 0, 0, 1],
  });
  ensure(
    constraints.equal(
      ops.vdist(queue.startHandle.center, queue.endHandle.center),
      queueLength,
    ),
  );

  const bounces = 3;
  const start = queue.endHandle.center;
  const dir = ops.vnormalize(ops.vsub(queue.startHandle.center, start));
  for (let i = 0; i < bounces; i++) {
    const intersect = rayIntersectRect([]);
  }

  return await build();
};

export default function PoolDiagram() {
  const diagram = useDiagram(poolDiagram);
  return <Renderer diagram={diagram} />;
}
