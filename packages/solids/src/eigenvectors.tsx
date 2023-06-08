import { Num, Var, add, mul, neg, ops, variable } from "@penrose/core";
import MarkdownIt from "markdown-it";
import mdMJ from "markdown-it-mathjax3";
import { createMutable } from "solid-js/store";
import { numSignal } from "./util.js";

const md = MarkdownIt({
  // linkify: true,
  // breaks: true,
}).use(mdMJ);

const [ox, oy] = [330, 330];
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

const toCanvas = ([x, y]: Num[]) => [
  add(mul(x, w / 5), ox),
  add(neg(mul(y, h / 5)), oy),
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
  const vals = toCanvas(val).map(numSignal);
  const [lx, ly] = ops.vadd(toCanvas(val), [15, -15]).map(numSignal);
  return (
    <g>
      <text
        font-family={fontFamily}
        font-size={fontSize}
        stroke={"0"}
        fill={fill}
        x={lx()}
        y={ly()}
        style={{
          "user-select": "none",
        }}
      >
        {label}
      </text>
      <circle
        cx={vals[0]()}
        cy={vals[1]()}
        r={4}
        fill={"rgb(231, 76, 60)"}
      ></circle>
      {draggable && (
        <g
          transform={`translate(${vals[0]()}, ${vals[1]()})`}
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
  const vals = toCanvas(val).map(numSignal);
  const [lx, ly] = ops.vadd(toCanvas(val), [5, -5]).map(numSignal);
  return (
    <g>
      <Arrowhead id={id} fill={fill}></Arrowhead>
      <text
        font-family={fontFamily}
        font-size={fontSize}
        stroke={"0"}
        fill={fill}
        x={lx()}
        y={ly()}
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
        x2={vals[0]()}
        y2={vals[1]()}
      ></line>
      <g onMouseDown={(e) => onMouseDown(e, svg, val)}>
        <circle
          cx={vals[0]()}
          cy={vals[1]()}
          r="22.91"
          fill="#000"
          fill-opacity={0.1}
        ></circle>
      </g>
    </g>
  );
};

// TODO: shorten
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
        <path fill="none" stroke={stroke} d={`M-6,${ox - height}H0V${oy}H-6`} />
      </g>
    </g>
  );
};

const vec = (x: number, y: number): [Var, Var] => [
  createMutable(variable(x)),
  createMutable(variable(y)),
];

export default () => {
  const v1 = vec(1, 0.5);
  const v2 = vec(0.5, 1);
  const v = vec(2, 3);
  const A = ops.mtrans([v1, v2]);
  const Av = ops.mvmul(A, v);
  const vd = v.map(numSignal);
  const avd = Av.map(numSignal);
  const v1d = v1.map(numSignal);
  const v2d = v2.map(numSignal);
  const a1Color = "#3498db";
  const a2Color = "#2ecc71";
  const vColor = "#E74C3C";

  return (
    <div style={{ display: "flex" }}>
      <svg ref={svg} width={800} height={500}>
        <Axis
          origin={[ox, oy]}
          width={w}
          height={h}
          xRange={[0, 5]}
          yRange={[0, 5]}
        />
        <g>
          <Vector id={"primary"} fill={a1Color} val={v1} label={"a₁"} />
          <Vector id={"secondary"} fill={a2Color} val={v2} label={"a₂"} />
          <Point fill={vColor} val={v} label={"v"} draggable />
          <Point fill={vColor} val={Av} label={"Av"} />
        </g>
      </svg>
      <div>
        <$>{`\\textcolor{${a1Color}}{a_1} = [${v1d[0]().toFixed(
          2
        )}, ${v1d[1]().toFixed(2)}]`}</$>
        <$>{`\\textcolor{${a2Color}}{a_2} = [${v2d[0]().toFixed(
          2
        )}, ${v2d[1]().toFixed(2)}]`}</$>
        <$>{`\\textcolor{${vColor}}{v}= [${vd[0]().toFixed(
          2
        )}, ${vd[1]().toFixed(2)}]`}</$>
        <$>
          {`\\textcolor{${vColor}}{A} =  
\\begin{bmatrix}
\\textcolor{${a1Color}}{a_1,x} & \\textcolor{${a2Color}}{a_2,x} \\\\
\\textcolor{${a1Color}}{a_1,y} & \\textcolor{${a2Color}}{a_2,y} \\\\
\\end{bmatrix} =
\\begin{bmatrix}
${v1d[0]().toFixed(2)} & ${v2d[0]().toFixed(2)}\\\\
${v1d[1]().toFixed(2)} & ${v2d[1]().toFixed(2)}
\\end{bmatrix}
`}
        </$>
        <$>{`\\textcolor{${vColor}}{Av}= [${avd[0]().toFixed(
          2
        )}, ${avd[1]().toFixed(2)}]`}</$>
      </div>
    </div>
  );
};
