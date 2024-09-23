import {
  DiagramBuilder,
  Renderer,
  Vec2,
  canvas,
  constraints,
  useDiagram,
} from "@penrose/bloom";
import { add, div, mul, ops, sqrt } from "@penrose/core";
import markdownIt from "markdown-it";
import markdownItKatex from "markdown-it-katex";
import { useCallback, useEffect, useRef, useState } from "react";

const md = markdownIt();
md.use(markdownItKatex);

export const basisVectors = async (withEigenspace = false) => {
  const canvasWidth = 500,
    canvasHeight = 400;

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
    bindToInput,
    input,
  } = new DiagramBuilder(canvas(canvasWidth, canvasHeight), "");

  // Misc constants
  const origin: [number, number] = [-canvasHeight / 6, (-2 * canvasHeight) / 5];
  const axesWidth = (2 * canvasWidth) / 4;
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
    input({ init: 1 * xScale + origin[0], optimized: false }),
    input({ init: 0.4 * yScale + origin[1], optimized: false }),
  ];
  ihat.color = [0.2, 0.6, 0.86, 1];

  const jhat = BasisVector();
  jhat.label = "a_2";
  jhat.canvasVec = [
    input({ init: 0.5 * xScale + origin[0], optimized: false }),
    input({ init: 1 * yScale + origin[1], optimized: false }),
  ];
  jhat.color = [0.18, 0.8, 0.44, 1];

  const p = MappedPoint();
  p.canvasV1 = [
    input({ init: 1.5 * xScale + origin[0], optimized: false }),
    input({ init: 3.0 * yScale + origin[1], optimized: false }),
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

  bindToInput(ihat.vec[0], { name: "ihat.x" });
  bindToInput(ihat.vec[1], { name: "ihat.y" });
  bindToInput(jhat.vec[0], { name: "jhat.x" });
  bindToInput(jhat.vec[1], { name: "jhat.y" });

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

  bindToInput(p.v1[0], { name: "v.x" });
  bindToInput(p.v1[1], { name: "v.y" });
  bindToInput(p.v2[0], { name: "Av.x" });
  bindToInput(p.v2[1], { name: "Av.y" });

  if (withEigenspace) {
    const EigenspaceLine = type();

    const el1 = EigenspaceLine();
    const el2 = EigenspaceLine();

    const [a, c] = ihat.vec;
    const [b, d] = jhat.vec;

    el1.vec = ops.vnormalize([
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
    ]);

    el2.vec = ops.vnormalize([
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
    ]);

    bindToInput(el1.vec[0], { name: "s1.x" });
    bindToInput(el1.vec[1], { name: "s1.y" });
    bindToInput(el2.vec[0], { name: "s2.x" });
    bindToInput(el2.vec[1], { name: "s2.y" });

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
  const katexRef1 = useRef<HTMLDivElement>(null);
  const katexRef2 = useRef<HTMLDivElement>(null);
  const diagram = useDiagram(useCallback(() => basisVectors(true), []));
  const [ihatx, setIhatx] = useState(0);
  const [ihaty, setIhaty] = useState(0);
  const [jhatx, setJhatx] = useState(0);
  const [jhaty, setJhaty] = useState(0);
  const [s1x, setS1x] = useState(0);
  const [s1y, setS1y] = useState(0);
  const [s2x, setS2x] = useState(0);
  const [s2y, setS2y] = useState(0);
  const [vx, setVx] = useState(0);
  const [vy, setVy] = useState(0);
  const [Avx, setAvx] = useState(0);
  const [Avy, setAvy] = useState(0);

  useEffect(() => {
    const disableDarkMode = () => {
      try {
        const darkModeButton = document.getElementsByClassName("VPSwitch")[0];
        if (darkModeButton.getAttribute("title") === "Switch to light theme") {
          darkModeButton.click();
        }
      } catch {
        setTimeout(disableDarkMode, 100);
      }
    };
    setTimeout(disableDarkMode, 100);
  }, []);

  useEffect(() => {
    if (diagram) {
      diagram.addInputEffect("ihat.x", setIhatx);
      diagram.addInputEffect("ihat.y", setIhaty);
      diagram.addInputEffect("jhat.x", setJhatx);
      diagram.addInputEffect("jhat.y", setJhaty);
      diagram.addInputEffect("s1.x", setS1x);
      diagram.addInputEffect("s1.y", setS1y);
      diagram.addInputEffect("s2.x", setS2x);
      diagram.addInputEffect("s2.y", setS2y);
      diagram.addInputEffect("v.x", setVx);
      diagram.addInputEffect("v.y", setVy);
      diagram.addInputEffect("Av.x", setAvx);
      diagram.addInputEffect("Av.y", setAvy);
    }
  }, [diagram]);

  useEffect(() => {
    if (katexRef1.current && katexRef2.current) {
      katexRef1.current.innerHTML = md.render(
        `$$A = \\begin{bmatrix} ${ihatx.toFixed(2)} & ${jhatx.toFixed(
          2,
        )} \\\\ ${ihaty.toFixed(2)} & ${jhaty.toFixed(2)} \\end{bmatrix}$$`,
      );
      katexRef1.current.innerHTML += md.render(
        `$$s_1 = \\begin{bmatrix} ${s1x.toFixed(2)} \\\\ ${s1y.toFixed(
          2,
        )} \\end{bmatrix}$$`,
      );
      katexRef1.current.innerHTML += md.render(
        `$$s_2 = \\begin{bmatrix} ${s2x.toFixed(2)} \\\\ ${s2y.toFixed(
          2,
        )} \\end{bmatrix}$$`,
      );

      katexRef2.current.innerHTML = md.render(
        `$$v_1 = \\begin{bmatrix} ${vx.toFixed(2)} \\\\ ${vy.toFixed(
          2,
        )} \\end{bmatrix}$$`,
      );
      katexRef2.current.innerHTML += md.render(
        `$$Av_1 = \\begin{bmatrix} ${Avx.toFixed(2)} \\\\ ${Avy.toFixed(
          2,
        )} \\end{bmatrix}$$`,
      );
    }
  }, [ihatx, jhatx, ihaty, jhaty, vx, vy]);

  if (!diagram) {
    return <div>Loading...</div>;
  }

  return (
    <div
      style={{
        display: "flex",
        flexDirection: "row",
        justifyContent: "space-evenly",
        flexWrap: "wrap",
        height: "100%",
      }}
    >
      <div
        style={{
          minWidth: "60%",
          display: "flex",
          justifyContent: "center",
          // border: "1px solid black",
        }}
      >
        <Renderer diagram={diagram} />
      </div>
      <div
        style={{
          display: "flex",
          alignItems: "center",
          flexDirection: "column",
          justifyContent: "center",
          width: "18%",
        }}
        ref={katexRef2}
      ></div>
      <div
        style={{
          display: "flex",
          alignItems: "center",
          flexDirection: "column",
          justifyContent: "center",
          width: "18%",
        }}
        ref={katexRef1}
      ></div>
    </div>
  );
}
