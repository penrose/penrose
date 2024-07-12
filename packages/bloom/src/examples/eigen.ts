import { mul, ops } from "@penrose/core";
import { DiagramBuilder } from "../builder/builder.js";
import constraints from "../lib/constraints.ts";
import { Vec2 } from "../types.ts";
import { canvas } from "../utils.js";

export const basisVectors = async () => {
  const canvasWidth = 800,
    canvasHeight = 800;

  const {
    type,
    predicate,
    substance,
    line,
    circle,
    equation,
    build,
    forall,
    ensure,
    forallWhere,
    vary,
    encourage,
    layer,
  } = new DiagramBuilder(canvas(canvasWidth, canvasHeight), "");

  const origin: [number, number] = [-canvasWidth / 3, -canvasHeight / 3];
  const width = (2 * canvasWidth) / 3;
  const height = (2 * canvasWidth) / 3;
  const topLeft: [number, number] = [origin[0], origin[1] + height];
  const bottomRight: [number, number] = [origin[0] + width, origin[1]];

  // Axes
  const Axis = type();
  const Horizontal = predicate();
  const Vertical = predicate();

  const TickMark = type();
  const OnAxis = predicate();

  const BasisVector = type();
  const MappedPoint = type();

  // substance
  const xAxis = Axis();
  Horizontal(xAxis);

  const yAxis = Axis();
  Vertical(yAxis);

  const numTickMarks = 6;
  for (let i = 0; i < numTickMarks; ++i) {
    const xTick = TickMark();
    OnAxis(xTick, xAxis);

    const yTick = TickMark();
    OnAxis(yTick, yAxis);
  }

  const ihat = BasisVector();
  ihat.label = "a_1";
  ihat.vec = [vary({ init: 1, pinned: true }), vary({ init: 0, pinned: true })];
  ihat.color = [0.2, 0.6, 0.86, 1];

  const jhat = BasisVector();
  jhat.label = "a_2";
  jhat.vec = [
    vary({ init: 0.5, pinned: true }),
    vary({ init: 1, pinned: true }),
  ];
  jhat.color = [0.18, 0.8, 0.44, 1];

  const numMappedPoints = 2;
  for (let i = 0; i < numMappedPoints; ++i) {
    const p = MappedPoint();
    p.v1 = [
      vary({ init: i + 2, pinned: true }),
      vary({ init: i + 2, pinned: true }),
    ];
  }

  // Style

  // Draw axes
  forall({ a: Axis }, ({ a }) => {
    if (Horizontal.test(a)) {
      a.icon = line({
        start: origin,
        end: bottomRight,
      });
    } else if (Vertical.test(a)) {
      a.icon = line({
        start: origin,
        end: topLeft,
      });
    }
  });

  const xScale = width / (numTickMarks - 1);
  const yScale = height / (numTickMarks - 1);

  // draw horizontal tick marks
  forallWhere(
    { t: TickMark, a: Axis },
    ({ t, a }) => OnAxis.test(t, a) && Horizontal.test(a),
    ({ t }, n) => {
      const len = 10;
      const start: [number, number] = [origin[0] + n * xScale, origin[1]];
      const end: [number, number] = [start[0], start[1] - len];

      t.icon = line({
        start,
        end,
      });

      t.text = equation({
        string: `${n}`,
        center: ops.vadd(end, [0, -10]) as Vec2,
      });
    },
  );

  // draw vertical tick marks
  forallWhere(
    { t: TickMark, a: Axis },
    ({ t, a }) => OnAxis.test(t, a) && Vertical.test(a),
    ({ t }, n) => {
      const len = 10;
      const start: [number, number] = [origin[0], origin[1] + n * yScale];
      const end: [number, number] = [start[0] - len, start[1]];

      t.icon = line({
        start,
        end,
      });

      t.text = equation({
        string: `${n}`,
        center: ops.vadd(end, [-10, 0]) as Vec2,
      });
    },
  );

  // draw all basis vectors
  forall({ v: BasisVector }, ({ v }) => {
    v.canvasVec = ops.vadd(origin, [
      mul(v.vec[0], xScale),
      mul(v.vec[1], yScale),
    ]);

    v.arrow = line({
      start: origin,
      end: v.canvasVec,
      strokeWidth: 5,
      endArrowhead: "straight",
      endArrowheadSize: 0.4,
      strokeColor: v.color,
    });

    v.handle = circle({
      r: 30,
      fillColor: [0, 0, 0, 0.1],
      center: v.canvasVec,
    });

    v.text = equation({
      string: v.label,
      fontSize: "24px",
      fillColor: v.color,
    });

    ensure(constraints.disjoint(v.text, v.arrow, 5));
    ensure(constraints.lessThan(ops.vdist(v.text.center, v.canvasVec), 15));
  });

  // color basis vectors

  // draw points, their maps, and dots between them
  forall({ p: MappedPoint }, ({ p }, n) => {
    p.v2 = ops.mvmul(ops.mtrans([ihat.vec, jhat.vec]), p.v1) as Vec2;

    p.canvasV1 = ops.vadd(origin, [mul(p.v1[0], xScale), mul(p.v1[1], yScale)]);
    p.canvasV2 = ops.vadd(origin, [mul(p.v2[0], xScale), mul(p.v2[1], yScale)]);

    p.color = [0.91, 0.3, 0.24, 1];

    p.icon1 = circle({
      r: 5,
      fillColor: p.color,
      center: p.canvasV1,
    });

    p.text1 = equation({
      string: `v_${n + 1}`,
      fontSize: "24px",
      fillColor: p.color,
    });

    p.icon2 = circle({
      r: 5,
      fillColor: p.color,
      center: p.canvasV2,
    });

    p.text2 = equation({
      string: `Av_${n + 1}`,
      fontSize: "24px",
      fillColor: p.color,
    });

    p.line = line({
      start: p.canvasV1,
      end: p.canvasV2,
      strokeStyle: "dashed",
    });

    p.handle = circle({
      r: 30,
      center: p.canvasV1,
      fillColor: [0, 0, 0, 0.1],
    });

    ensure(constraints.disjoint(p.text1, p.icon1, 5));
    ensure(constraints.disjoint(p.text1, p.icon2, 5));
    ensure(constraints.disjoint(p.text2, p.icon1, 5));
    ensure(constraints.disjoint(p.text2, p.icon2, 5));
    ensure(constraints.disjoint(p.text1, p.text2, 5));
    ensure(constraints.disjoint(p.text1, p.line, 5));
    ensure(constraints.disjoint(p.text2, p.line, 5));
    ensure(constraints.lessThan(ops.vdist(p.text1.center, p.canvasV1), 15));
    ensure(constraints.lessThan(ops.vdist(p.text2.center, p.canvasV2), 15));

    layer(p.line, p.icon1);
  });

  return await build();
};
