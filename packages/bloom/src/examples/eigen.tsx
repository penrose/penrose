import { add, div, mul, ops, sqrt } from "@penrose/core";
import { useEffect, useMemo, useState } from "react";
import { DiagramBuilder } from "../builder/builder.js";
import { Diagram } from "../builder/diagram.ts";
import Renderer from "../components/Renderer.tsx";
import constraints from "../lib/constraints.ts";
import { useSharedInputs } from "../lib/hooks.ts";
import { SharedInput, Vec2 } from "../types.ts";
import { canvas } from "../utils.js";

const basisVectors = async (
  inputs: {
    ihatx: SharedInput;
    ihaty: SharedInput;
    jhatx: SharedInput;
    jhaty: SharedInput;
    px: SharedInput;
    py: SharedInput;
  },
  withEigenspace = false,
) => {
  const canvasWidth = 800,
    canvasHeight = 800;

  const {
    type,
    predicate,
    line,
    circle,
    equation,
    build,
    forall,
    ensure,
    forallWhere,
    layer,
    sharedInput,
  } = new DiagramBuilder(canvas(canvasWidth, canvasHeight), "");

  // Misc constants
  const origin: [number, number] = [-canvasHeight / 3, -canvasHeight / 3];
  const axesWidth = (2 * canvasWidth) / 3;
  const axesHeight = (2 * canvasWidth) / 3;
  const topLeft: [number, number] = [origin[0], origin[1] + axesHeight];
  const bottomRight: [number, number] = [origin[0] + axesWidth, origin[1]];

  const numTickMarks = 6;
  const xScale = axesWidth / (numTickMarks - 1);
  const yScale = axesHeight / (numTickMarks - 1);

  // Domain
  const Axis = type();
  const Horizontal = predicate();
  const Vertical = predicate();

  const TickMark = type();
  const OnAxis = predicate();

  const BasisVector = type();
  const MappedPoint = type();

  // Substance
  const xAxis = Axis();
  Horizontal(xAxis);

  const yAxis = Axis();
  Vertical(yAxis);

  for (let i = 0; i < numTickMarks; ++i) {
    const xTick = TickMark();
    OnAxis(xTick, xAxis);

    const yTick = TickMark();
    OnAxis(yTick, yAxis);
  }

  // Since there is no difference between style and substance programs,
  // we can associate data that is specific to each substance when we define it
  const ihat = BasisVector();
  ihat.label = "a_1";
  ihat.canvasVec = [
    sharedInput(inputs.ihatx, 1 * xScale + origin[0]),
    sharedInput(inputs.ihaty, 0.4 * yScale + origin[1]),
  ];
  ihat.color = [0.2, 0.6, 0.86, 1];

  const jhat = BasisVector();
  jhat.label = "a_2";
  jhat.canvasVec = [
    sharedInput(inputs.jhatx, 0.5 * xScale + origin[0]),
    sharedInput(inputs.jhaty, 1 * yScale + origin[1]),
  ];
  jhat.color = [0.18, 0.8, 0.44, 1];

  const p = MappedPoint();
  p.canvasV1 = [
    sharedInput(inputs.px, 1.5 * xScale + origin[0]),
    sharedInput(inputs.py, 1.5 * yScale + origin[1]),
  ];

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
    v.vec = ops.ewvvdiv(ops.vsub(v.canvasVec, origin), [xScale, yScale]);

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
      drag: true,
      dragConstraint: ([x, y]) => [
        Math.max(origin[0], x),
        Math.max(origin[1], y),
      ],
    });

    v.text = equation({
      string: v.label,
      fontSize: "24px",
      fillColor: v.color,
    });

    ensure(constraints.disjoint(v.text, v.arrow, 5));
    ensure(constraints.contains(v.handle, v.text));
    ensure(constraints.greaterThan(v.vec[0], origin[0]));
    ensure(constraints.greaterThan(v.vec[1], origin[1]));
  });

  // draw points, their maps, and dots between them
  forall({ p: MappedPoint }, ({ p }, n) => {
    p.v1 = ops.ewvvdiv(ops.vsub(p.canvasV1, origin), [xScale, yScale]);
    p.v2 = ops.mvmul(ops.mtrans([ihat.vec, jhat.vec]), p.v1);

    p.canvasV2 = ops.vadd(ops.ewvvmul(p.v2, [xScale, yScale]), origin);

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
      ensureOnCanvas: false,
    });

    p.text2 = equation({
      string: `Av_${n + 1}`,
      fontSize: "24px",
      fillColor: p.color,
      ensureOnCanvas: false,
    });

    p.line = line({
      start: p.canvasV1,
      end: p.canvasV2,
      strokeStyle: "dashed",
      ensureOnCanvas: false,
    });

    p.handle = circle({
      r: 30,
      fillColor: [0, 0, 0, 0.1],
      center: p.canvasV1,
      drag: true,
      dragConstraint: ([x, y]) => [
        Math.max(origin[0], x),
        Math.max(origin[1], y),
      ],
    });

    ensure(constraints.disjoint(p.text1, p.icon1, 5));
    ensure(constraints.disjoint(p.text1, p.icon2, 5));
    ensure(constraints.disjoint(p.text2, p.icon1, 5));
    ensure(constraints.disjoint(p.text2, p.icon2, 5));
    ensure(constraints.disjoint(p.text1, p.text2, 5));
    ensure(constraints.disjoint(p.text1, p.line, 5));
    ensure(constraints.disjoint(p.text2, p.line, 5));
    ensure(constraints.contains(p.handle, p.text1));
    ensure(constraints.lessThan(ops.vdist(p.text2.center, p.canvasV2), 15));

    layer(p.line, p.icon1);
  });

  if (withEigenspace) {
    const EigenspaceLine = type();

    const el1 = EigenspaceLine();
    const el2 = EigenspaceLine();

    const [a, c] = ihat.vec;
    const [b, d] = jhat.vec;

    el1.vec = [
      div(
        mul(
          -1,
          add(
            add(mul(-1, a), d),
            sqrt(
              add(
                add(add(mul(a, a), mul(4, mul(b, c))), mul(-2, mul(a, d))),
                mul(d, d),
              ),
            ),
          ),
        ),
        mul(2, c),
      ),
      1,
    ];

    el2.vec = [
      div(
        mul(
          -1,
          add(
            add(mul(-1, a), d),
            mul(
              -1,
              sqrt(
                add(
                  add(add(mul(a, a), mul(4, mul(b, c))), mul(-2, mul(a, d))),
                  mul(d, d),
                ),
              ),
            ),
          ),
        ),
        mul(2, c),
      ),
      1,
    ];

    forall({ e: EigenspaceLine }, ({ e }, i) => {
      e.icon = line({
        start: origin,
        end: ops.vadd(ops.vmul(1000, e.vec), origin) as Vec2,
        strokeWidth: 2,
      });

      e.text = equation({
        string: `s_${i + 1}`,
        center: ops.vadd(ops.vmul(200, e.vec), origin) as Vec2,
        fontSize: "24px",
      });

      e.textBackground = circle({
        r: 20,
        fillColor: [1, 1, 1, 1],
        center: ops.vadd(ops.vmul(200, e.vec), origin) as Vec2,
      });

      layer(e.icon, e.textBackground);
      layer(e.textBackground, e.text);
    });

    forall({ e: EigenspaceLine, a: Axis }, ({ e, a }) => {
      layer(e.textBackground, a.icon);
    });
  }

  return await build();
};

export default function EigenvectorsDiagram() {
  const [diagram1, setDiagram1] = useState<Diagram | null>(null);
  const [diagram2, setDiagram2] = useState<Diagram | null>(null);

  const [ihatx, ihaty, jhatx, jhaty, px, py] = useSharedInputs(6);

  const inputs = useMemo(
    () => ({
      ihatx,
      ihaty,
      jhatx,
      jhaty,
      px,
      py,
    }),
    [ihatx, ihaty, jhatx, jhaty, px, py],
  );

  useEffect(() => {
    basisVectors(inputs, false).then(setDiagram1);
    basisVectors(inputs, true).then(setDiagram2);
  }, [inputs]);

  if (!diagram1 || !diagram2) {
    return <div>Loading...</div>;
  }

  return (
    <div
      style={{
        display: "flex",
        flexDirection: "row",
        justifyContent: "space-evenly",
        height: "100%",
      }}
    >
      <Renderer diagram={diagram1} />
      <Renderer diagram={diagram2} />
    </div>
  );
}