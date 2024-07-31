import { mul, ops } from "@penrose/core";
import { DiagramBuilder, Substance, Vec2, canvas } from "../../lib";
import computation from "../../lib/core/computation.ts";
import constraints from "../../lib/core/constraints.ts";

const drawSource = (db: DiagramBuilder, source: Substance) => {
  const { polyline, input, ensure, circle, layer, bindToInput } = db;

  const angle = (2 * Math.PI) / 3;
  const borderRad = 20;
  const r1 = computation
    .rotate(-angle / 2)
    .map((v) => v.slice(0, 2))
    .slice(0, 2);
  const r2 = computation
    .rotate(Math.PI)
    .map((v) => v.slice(0, 2))
    .slice(0, 2);
  const r3 = computation
    .rotate(angle / 2)
    .map((v) => v.slice(0, 2))
    .slice(0, 2);

  source.center = [
    input({ init: -300, pinned: true }),
    input({ init: 100, pinned: true }),
  ];
  source.heading = ops.vnormalize([input(), input()]);

  const bp1 = ops.vadd(
    source.center,
    ops.vmul(borderRad, ops.mvmul(r1, source.heading)),
  ) as Vec2;
  const bp2 = ops.vadd(
    source.center,
    ops.vmul(borderRad, ops.mvmul(r2, source.heading)),
  ) as Vec2;
  const bp3 = ops.vadd(
    source.center,
    ops.vmul(borderRad, ops.mvmul(r3, source.heading)),
  ) as Vec2;

  const borderPoints = [bp1, bp2, bp3];
  source.border = polyline({
    points: borderPoints,
    strokeWidth: 2,
  });

  source.handle = circle({
    center: source.center,
    r: 40,
    drag: true,
    fillColor: [0, 0, 0, 0.1],
  });
};

export const reflection = async (userInput: string) => {
  const db = new DiagramBuilder(canvas(800, 400), "");
  const { type, line, circle, build, input, ensure, forall } = db;

  const Source = type();
  const Target = type();
  const Ray = type();
  const Mirror = type();

  const source = Source();
  const target = Target();
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

  // draw source and target
  drawSource(db, source);

  target.center = [
    input({ init: 300, pinned: true }),
    input({ init: 100, pinned: true }),
  ];
  target.dot = circle({
    center: target.center,
    r: 5,
    fillColor: [0, 0, 0, 1],
  });
  target.handle = circle({
    center: target.center,
    r: 40,
    drag: true,
    fillColor: [0, 0, 0, 0.1],
  });

  // draw rays
  const reflectPoint = [input({ init: 0 }), mirror.level];

  ray1.start = [input(), input()];
  ray1.end = reflectPoint;

  ray2.start = reflectPoint;
  ray2.end = [input(), input()];

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

  eval(userInput);

  const r1x = ray1.normVec[0];
  const r2x = ray2.normVec[0];
  ensure(constraints.equal(r1x, mul(1, r2x)));

  ensure(
    constraints.equal(
      ops.vdot(ray1.normVec, ops.vsub(reflectPoint, source.center)),
      ops.vdist(reflectPoint, source.center),
    ),
    10,
  );
  ensure(
    constraints.equal(
      ops.vdot(ray2.normVec, ops.vsub(target.center, reflectPoint)),
      ops.vdist(target.center, reflectPoint),
    ),
    10,
  );
  ensure(constraints.equal(ops.vdist(ray1.start, source.center), 40));
  ensure(constraints.equal(ops.vdist(ray2.end, target.center), 40));
  ensure(constraints.equal(source.heading[0], ray1.normVec[0]), 5);
  ensure(constraints.equal(source.heading[1], ray1.normVec[1]), 5);
  ensure(
    constraints.lessThan(
      ops.vdist(ray1.start, ray1.end),
      ops.vdist(source.center, reflectPoint),
    ),
  );
  ensure(
    constraints.lessThan(
      ops.vdist(ray2.start, ray2.end),
      ops.vdist(reflectPoint, target.center),
    ),
  );

  return await build();
};
