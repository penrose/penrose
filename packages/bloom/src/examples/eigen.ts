import { mul, ops } from "@penrose/core";
import { DiagramBuilder } from "../builder/builder.js";
import constraints from "../lib/constraints.ts";
import objectives from "../lib/objectives.ts";
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
  ihat.label = "\\hat i";

  const jhat = BasisVector();
  jhat.label = "\\hat j";

  const numMappedPoints = 1;
  for (let i = 0; i < numMappedPoints; ++i) {
    const p = MappedPoint();
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

  const ensureInSquare = (v: Vec2) => {
    ensure(constraints.lessThan(0, v[0]));
    ensure(constraints.lessThan(v[0], numTickMarks - 1));

    ensure(constraints.lessThan(0, v[1]));
    ensure(constraints.lessThan(v[1], numTickMarks - 1));
  };

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
    },
  );

  // draw all basis vectors
  forall({ v: BasisVector }, ({ v }) => {
    v.vec = [vary(), vary()];
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
    });

    v.handle = circle({
      r: 30,
      fillColor: [0, 0, 0, 0.2],
      center: v.canvasVec,
    });

    v.text = equation({
      string: v.label,
      fontSize: "24px",
    });

    ensureInSquare(v.vec);
    ensure(constraints.disjoint(v.text, v.handle, 5));
    ensure(constraints.disjoint(v.text, v.arrow, 5));

    encourage(objectives.repelPt(1, v.vec, [0, 0]));
    encourage(objectives.minimal(v.vec[0]));
    encourage(objectives.minimal(v.vec[1]));

    encourage(objectives.nearPt(v.text, v.canvasVec[0], v.canvasVec[1]));
  });

  // color basis vectors
  ihat.arrow.strokeColor = [0, 0, 1, 1];
  jhat.arrow.strokeColor = [0, 1, 0, 1];

  // draw points, their maps, and dots between them
  forall({ p: MappedPoint }, ({ p }) => {
    p.v1 = [vary(), vary()];
    p.v2 = ops.mvmul(ops.mtrans([ihat.vec, jhat.vec]), p.v1) as Vec2;

    p.canvasV1 = ops.vadd(origin, [mul(p.v1[0], xScale), mul(p.v1[1], yScale)]);
    p.canvasV2 = ops.vadd(origin, [mul(p.v2[0], xScale), mul(p.v2[1], yScale)]);

    p.icon1 = circle({
      r: 5,
      fillColor: [0, 0, 0, 1],
      center: p.canvasV1,
    });

    p.icon2 = circle({
      r: 5,
      fillColor: [0, 0, 0, 1],
      center: p.canvasV2,
    });

    p.line = line({
      start: p.canvasV1,
      end: p.canvasV2,
      strokeStyle: "dashed",
    });

    p.handle = circle({
      r: 30,
      center: p.canvasV1,
      fillColor: [0, 0, 0, 0.2],
    });

    ensureInSquare(p.v1);
  });

  return await build();
};
