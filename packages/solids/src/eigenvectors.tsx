import {
  Num,
  Var,
  add,
  div,
  mul,
  neg,
  ops,
  sqrt,
  sub,
  variable,
} from "@penrose/core";
import MarkdownIt from "markdown-it";
import mdMJ from "markdown-it-mathjax3";
import { createMutable } from "solid-js/store";
import { num, signalNum } from "./util.js";

const md = MarkdownIt({
  // linkify: true,
  // breaks: true,
}).use(mdMJ);

const [ox, oy] = [200, 330];
const [w, h] = [270, 270];
const fontSize = "20px";
const fontFamily = "STIXGeneral-Italic";
let svg: SVGSVGElement;

const Draggable = (props: any) => {
  return <div>draggable</div>;
};

const $ = (props: { children: string }) => (
  <span innerHTML={md.render(`$${props.children}$`)}></span>
);
const P = (props: { children: string }) => (
  <p innerHTML={md.render(`${props.children}`)}></p>
);

const toCanvas = (props: Num[]) => [
  add(mul(props[0], w / 5), ox),
  add(neg(mul(props[1], h / 5)), oy),
];
const toModel = ([x, y]: number[]): number[] => [x / (w / 5), y / (h / 5)];

const Arrowhead = (args: { id: string; fill: string }) => (
  <marker
    orient="auto"
    markerWidth="8"
    markerHeight="16"
    refX="1.5"
    refY="2"
    {...args}
  >
    <path d="M 0,0 V4 L2,2 Z"></path>
  </marker>
);

const Point = ({
  val,
  fill,
  label,
  draggable,
}: {
  val: Num[];
  fill: string;
  label: string;
  draggable?: boolean;
}) => {
  const vals = toCanvas(val).map(signalNum);
  const [lx, ly] = ops.vadd(toCanvas(val), [15, -15]).map(signalNum);
  return (
    <g>
      <text
        font-family={fontFamily}
        font-size={fontSize}
        stroke={"0"}
        fill={fill}
        x={num(lx)}
        y={num(ly)}
        style={{
          "user-select": "none",
        }}
      >
        {label}
      </text>
      <circle
        cx={num(vals[0])}
        cy={num(vals[1])}
        r={4}
        fill={"rgb(231, 76, 60)"}
      ></circle>
      {draggable && (
        <g
          transform={`translate(${num(vals[0])}, ${num(vals[1])})`}
          onMouseDown={(e) => onMouseDown(e, svg, val as Var[])}
        >
          <circle r="22.91" fill="#000" fill-opacity={0.1}></circle>
        </g>
      )}
    </g>
  );
};

/**
 * Converts screen to relative SVG coords
 * Thanks to
 * https://www.petercollingridge.co.uk/tutorials/svg/interactive/dragging/
 * @param e
 * @param svg
 */
const getPosition = (
  { clientX, clientY }: { clientX: number; clientY: number },
  svg: SVGSVGElement
) => {
  const CTM = svg.getScreenCTM();
  if (CTM !== null) {
    return { x: (clientX - CTM.e) / CTM.a, y: (clientY - CTM.f) / CTM.d };
  }
  return { x: 0, y: 0 };
};

const onMouseDown = (e: MouseEvent, parent: SVGSVGElement, val: Var[]) => {
  const target = e.target as SVGSVGElement;
  target.setAttribute("fill-opacity", "0.15");
  const { clientX, clientY } = e;
  let { x: tempX, y: tempY } = getPosition({ clientX, clientY }, parent);
  let dx = 0,
    dy = 0;
  const onMouseMove = (e: MouseEvent) => {
    const { x, y } = getPosition(e, parent);
    dx = x - tempX;
    dy = tempY - y;
    tempX = x;
    tempY = y;
    // set the actual values
    const [mx, my] = toModel([dx, dy]);
    const futureX = val[0].val + mx;
    const futureY = val[1].val + my;
    if (futureX >= 0 && futureX <= 5) val[0].val = futureX;
    if (futureY >= 0 && futureY <= 5) val[1].val = futureY;
  };
  const onMouseUp = (e: MouseEvent) => {
    document.removeEventListener("mouseup", onMouseUp);
    document.removeEventListener("mousemove", onMouseMove);
    // remove transform
    target.setAttribute(`transform`, "");
    target.setAttribute("fill-opacity", "0.1");
  };
  document.addEventListener("mouseup", onMouseUp);
  document.addEventListener("mousemove", onMouseMove);
};

const Vector = ({
  id,
  fill,
  val,
  label,
}: {
  val: Var[];
  id: string;
  fill: string;
  label: string;
}) => {
  const vals = toCanvas(val).map(signalNum);
  const [lx, ly] = ops.vadd(toCanvas(val), [5, -5]).map(signalNum);
  return (
    <g>
      <Arrowhead id={id} fill={fill}></Arrowhead>
      <text
        font-family={fontFamily}
        font-size={fontSize}
        stroke={"0"}
        fill={fill}
        x={num(lx)}
        y={num(ly)}
        style={{
          "user-select": "none",
        }}
      >
        {label}
      </text>
      <line
        marker-end={`url(#${id})`}
        stroke-width={6}
        stroke={fill}
        x1={ox}
        y1={oy}
        x2={num(vals[0])}
        y2={num(vals[1])}
      ></line>
      <g onMouseDown={(e) => onMouseDown(e, svg, val)}>
        <circle
          cx={num(vals[0])}
          cy={num(vals[1])}
          r="22.91"
          fill="#000"
          fill-opacity={0.1}
        ></circle>
      </g>
    </g>
  );
};

const Axis = ({
  origin: [ox, oy],
  xRange: [minX, maxX],
  yRange: [minY, maxY],
  width,
  height,
}: {
  origin: [number, number];
  xRange: [number, number];
  yRange: [number, number];
  width: number;
  height: number;
}) => {
  const xStep = width / (maxX - minX);
  const yStep = height / (maxY - minY);
  const xTicks = Math.floor(width / xStep) + 1;
  const yTicks = Math.floor(height / yStep) + 1;
  const stroke = "#0002";
  return (
    <g
      style={{
        "user-select": "none",
      }}
    >
      <g
        transform={`translate(0,${oy})`}
        style="font-size: 10px; pointer-events: none;"
      >
        {Array.from({ length: xTicks }).map((_, i: number) => (
          <g transform={`translate(${ox + i * xStep},0)`} style="opacity: 1;">
            <line y2="6" x2="0" stroke={stroke}></line>
            <text y="9" x="0" dy=".71em" style="text-anchor: middle;">
              {i}
            </text>
          </g>
        ))}
        <path fill="none" stroke="#0002" d={`M${ox},6V0H${ox + width}V6`} />
      </g>
      <g
        transform={`translate(${ox},0)`}
        style="font-size: 10px; pointer-events: none;"
      >
        {Array.from({ length: yTicks }).map((_, i: number) => (
          <g transform={`translate(0,${oy - i * yStep})`} style="opacity: 1;">
            <line x2="-6" y2="0" stroke={stroke}></line>
            <text x="-9" y="0" dy=".32em" style="text-anchor: end;">
              {i}
            </text>
          </g>
        ))}
        <path fill="none" stroke={stroke} d={`M-6,${oy}H0V${oy - height}H-6`} />
      </g>
    </g>
  );
};

const vec = (x: number, y: number): [Var, Var] => [
  createMutable(variable(x)),
  createMutable(variable(y)),
];

export const EigenValues = ({
  a1,
  a2,
  v,
}: {
  a1: Var[];
  a2: Var[];
  v: Var[];
}) => {
  const a1Color = "#3498db";
  const a2Color = "#2ecc71";
  const vColor = "#E74C3C";
  const A = ops.mtrans([a1, a2]);
  const Av = ops.mvmul(A, v);
  const vc = toCanvas(v).map(signalNum);
  const avc = toCanvas(ops.mvmul(A, v)).map(signalNum);
  // compute eigenvalues using the trick: https://www.youtube.com/watch?v=e50Bj7jn9IQ
  const m = div(add(a1[0], a2[1]), 2);
  const p = ops.cross2(A[0], A[1]);
  const d = sqrt(sub(mul(m, m), p));
  const eigenValues = [add(m, d), sub(m, d)];
  const eigenValuesD = eigenValues.map(signalNum);
  const eigen1 = ops.vnormalize([sub(eigenValues[0], A[1][1]), A[1][0]]);
  const eigen1D = eigen1.map(signalNum);
  const eigen2 = ops.vnormalize([sub(eigenValues[1], A[1][1]), A[1][0]]);
  const eigen2D = eigen2.map(signalNum);

  return (
    <div
      style={{
        display: "flex",
        width: "100%",
        "justify-content": "center",
        "align-content": "center",
      }}
    >
      <svg ref={svg} width={700} height={400}>
        <Axis
          origin={[ox, oy]}
          width={w}
          height={h}
          xRange={[0, 5]}
          yRange={[0, 5]}
        />
        <g>
          <Vector id={"primary"} fill={a1Color} val={a1} label={"a₁"} />
          <Vector id={"secondary"} fill={a2Color} val={a2} label={"a₂"} />
          <Point fill={vColor} val={v} label={"v"} draggable />
          <line
            x1={num(vc[0])}
            y1={num(vc[1])}
            x2={num(avc[0])}
            y2={num(avc[1])}
            stroke-width={2}
            stroke={"#0002"}
            stroke-dasharray={"2,2"}
          ></line>
          <Point fill={vColor} val={Av} label={"Av"} />
        </g>
        <line
          x1={ox}
          y1={oy}
          x2={ox + num(eigen1D[0]) * 10000}
          y2={oy - num(eigen1D[1]) * 10000}
          stroke-width={1}
          stroke={"#000"}
        ></line>
        <text
          font-family={fontFamily}
          font-size={fontSize}
          stroke={"0"}
          fill={"#000"}
          x={ox + num(eigen1D[0]) * 100}
          y={oy - num(eigen1D[1]) * 100}
          style={{
            "user-select": "none",
          }}
        >
          {"s₁"}
        </text>
        <line
          x1={ox}
          y1={oy}
          x2={ox + num(eigen2D[0]) * 10000}
          y2={oy - num(eigen2D[1]) * 10000}
          stroke-width={1}
          stroke={"#000"}
        ></line>
        <text
          font-family={fontFamily}
          font-size={fontSize}
          stroke={"0"}
          fill={"#000"}
          x={ox + num(eigen2D[0]) * 100}
          y={oy - num(eigen2D[1]) * 100}
          style={{
            "user-select": "none",
          }}
        >
          {"s₂"}
        </text>
      </svg>
      <div>
        <$>{`\\lambda_1 = ${num(eigenValuesD[0]).toFixed(2)}`}</$>
        <$>{`\\lambda_2 = ${num(eigenValuesD[1]).toFixed(2)}`}</$>
      </div>
    </div>
  );
};

export const Vectors = ({ a1, a2, v }: { a1: Var[]; a2: Var[]; v: Var[] }) => {
  const A = ops.mtrans([a1, a2]);
  const Av = ops.mvmul(A, v);
  const vd = v.map(signalNum);
  const avd = Av.map(signalNum);
  const v1d = a1.map(signalNum);
  const v2d = a2.map(signalNum);
  const vc = toCanvas(v).map(signalNum);
  const avc = toCanvas(ops.mvmul(A, v)).map(signalNum);
  const a1Color = "#3498db";
  const a2Color = "#2ecc71";
  const vColor = "#E74C3C";

  return (
    <div style={{ display: "flex", "align-items": "center" }}>
      <svg ref={svg} width={800} height={400}>
        <Axis
          origin={[ox, oy]}
          width={w}
          height={h}
          xRange={[0, 5]}
          yRange={[0, 5]}
        />
        <g>
          <Vector id={"primary"} fill={a1Color} val={a1} label={"a₁"} />
          <Vector id={"secondary"} fill={a2Color} val={a2} label={"a₂"} />
          <Point fill={vColor} val={v} label={"v"} draggable />
          <line
            x1={num(vc[0])}
            y1={num(vc[1])}
            x2={num(avc[0])}
            y2={num(avc[1])}
            stroke-width={2}
            stroke={"#0002"}
            stroke-dasharray={"2,2"}
          ></line>
          <Point fill={vColor} val={Av} label={"Av"} />
        </g>
      </svg>
      <div>
        <$>
          {`\\textcolor{${vColor}}{A} =  
\\begin{bmatrix}
\\textcolor{${a1Color}}{a_1,x} & \\textcolor{${a2Color}}{a_2,x} \\\\
\\textcolor{${a1Color}}{a_1,y} & \\textcolor{${a2Color}}{a_2,y} \\\\
\\end{bmatrix} =
\\begin{bmatrix}
${num(v1d[0]).toFixed(2)} & ${num(v2d[0]).toFixed(2)}\\\\
${num(v1d[1]).toFixed(2)} & ${num(v2d[1]).toFixed(2)}
\\end{bmatrix}
`}
        </$>
        <$>{`\\textcolor{${vColor}}{v}= [${num(vd[0]).toFixed(2)}, ${num(
          vd[1]
        ).toFixed(2)}]`}</$>
        <$>{`\\textcolor{${vColor}}{Av}= [${num(avd[0]).toFixed(2)}, ${num(
          avd[1]
        ).toFixed(2)}]`}</$>
      </div>
    </div>
  );
};

export default () => {
  const a1 = vec(1, 0.5);
  const a2 = vec(0.5, 1);
  const v = vec(2, 3);
  return (
    <div
      style={{ display: "flex", width: "100%", "justify-content": "center" }}
    >
      <div style={{ "font-family": "Lato", margin: "0 150px" }}>
        <h1
          style={{
            "font-size": "6em",
            "font-weight": "normal",
            "margin-bottom": 0,
          }}
        >
          Eigenvectors and Eigenvalues
          <br />
        </h1>
        <span
          style={{ "font-size": "1.5rem", "margin-top": 0, "font-weight": 100 }}
        >
          Explained Visually
        </span>
        <P>
          _Adapted from the original article:
          [https://setosa.io/ev/eigenvectors-and-eigenvalues/](https://setosa.io/ev/eigenvectors-and-eigenvalues/)_
        </P>
        <P>
          Eigenvalues/vectors are instrumental to understanding electrical
          circuits, mechanical systems, ecology and even Google's PageRank
          algorithm. Let's see if visualization can make these ideas more
          intuitive. To begin, let $v$ be a vector (shown as a point) and $A$ be
          a matrix with columns $a_1$ and $a_2$ (shown as arrows). If we
          multiply $v$ by $A$, then $A$ sends $v$ to a new vector $Av$.
        </P>
        <Vectors a1={a1} a2={a2} v={v} />
        <P>
          If you can draw a line through the three points $(0,0)$, $v$ and $Av$,
          then $Av$ is just $v$ multiplied by a number $\lambda$; that is, $Av =
          \lambda v$. In this case, we call $\lambda$ an __eigenvalue__ and $v$
          an __eigenvector__. For example, here $(1,2)$ is an eigvector and $5$
          an eigenvalue.
        </P>
        <P>
          Below, change the columns of $A$ and drag $v$ to be an eigenvector.
          Note three facts: First, every point on the same line as an
          eigenvector is an eigenvector. Those lines are __eigenspaces__, and
          each has an associated eigenvalue. Second, if you place $v$ on an
          eigenspace (either $s_1$ or $s_2$) with associated eigenvalue $\lambda
          &lt;1$, then $Av$ is closer to $(0,0)$ than $v$; but when $\lambda
          &gt;1$, it's farther. Third, both eigenspaces depend on both columns
          of $A$: it is not as though $a_1$ only affects $s_1$.
        </P>
        <EigenValues a1={a1} a2={a2} v={v} />
        <P>
          If you keep multiplying $v$ by $A$, you get a sequence $v, Av, A^2v$
          etc. Eigenspaces attract that sequence and eigenvalues tell you
          whether it ends up at $(0,0)$ or far away. Therefore,
          eigenvectors/values tell us about systems that evolve step-by-step.
        </P>
      </div>
    </div>
  );
};
