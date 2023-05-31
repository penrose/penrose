import { Num, Var, add, mul, neg, ops, variable } from "@penrose/core";
import { createMutable } from "solid-js/store";
import { numSignal } from "./util.js";

const [ox, oy] = [330, 270];
const [w, h] = [270, 270];
const fontSize = "20px";
const fontFamily = "STIXGeneral-Italic";
let svg: SVGSVGElement;

const Draggable = (props: any) => {
  return <div>draggable</div>;
};

const toCanvas = ([x, y]: Num[]) => [
  add(mul(x, w / 5), ox),
  add(neg(mul(y, h / 5)), oy),
];

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
      {draggable && (
        <g transform={`translate(${vals[0]()}, ${vals[1]()})`}>
          <circle r="22.91" fill="#000" fill-opacity={0.15}></circle>
        </g>
      )}
      <text
        font-family={fontFamily}
        font-size={fontSize}
        stroke={"0"}
        fill={fill}
        x={lx()}
        y={ly()}
      >
        {label}
      </text>
      <circle
        cx={vals[0]()}
        cy={vals[1]()}
        r={4}
        fill={"rgb(231, 76, 60)"}
      ></circle>
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

const clamp = (x: number, min: number, max: number): number =>
  Math.min(Math.max(x, min), max);

const onMouseDown = (e: MouseEvent, parent: SVGSVGElement) => {
  const { clientX, clientY } = e;
  const { x: tempX, y: tempY } = getPosition({ clientX, clientY }, parent);
  // const {
  //   width: bboxW,
  //   height: bboxH,
  //   x: bboxX,
  //   y: bboxY,
  // } = (e.target as SVGSVGElement).getBBox({ stroke: true });
  // const minX = tempX - bboxX;
  // const maxX = w - bboxW + (tempX - bboxX);
  // const minY = tempY - bboxY;
  // const maxY = h - bboxH + (tempY - bboxY);
  let dx = 0,
    dy = 0;
  const onMouseMove = (e: MouseEvent) => {
    const { x, y } = getPosition(e, parent);
    // const constrainedX = clamp(x, minX, maxX);
    // const constrainedY = clamp(y, minY, maxY);
    // dx = constrainedX - tempX;
    // dy = tempY - constrainedY;
    dx = x - tempX;
    dy = tempY - y;
    console.log(e.target);

    (e.target as SVGSVGElement).setAttribute(
      `transform`,
      `translate(${dx},${-dy})`
    );
  };
  const onMouseUp = () => {
    document.removeEventListener("mouseup", onMouseUp);
    document.removeEventListener("mousemove", onMouseMove);
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
      <g onMouseDown={(e) => onMouseDown(e, svg)}>
        <circle
          cx={vals[0]()}
          cy={vals[1]()}
          r="22.91"
          fill="#000"
          fill-opacity={0.15}
        ></circle>
      </g>
      <text
        font-family={fontFamily}
        font-size={fontSize}
        stroke={"0"}
        fill={fill}
        x={lx()}
        y={ly()}
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
    </g>
  );
};

// TODO: shorten
const Axis = () => (
  <g class="axis">
    <g
      class="axis"
      transform="translate(0,270)"
      style="font-size: 10px; pointer-events: none;"
    >
      <g class="tick" transform="translate(330,0)" style="opacity: 1;">
        <line y2="6" x2="0"></line>
        <text y="9" x="0" dy=".71em" style="text-anchor: middle;">
          0
        </text>
      </g>
      <g class="tick" transform="translate(390,0)" style="opacity: 1;">
        <line y2="6" x2="0"></line>
        <text y="9" x="0" dy=".71em" style="text-anchor: middle;">
          1
        </text>
      </g>
      <g class="tick" transform="translate(450,0)" style="opacity: 1;">
        <line y2="6" x2="0"></line>
        <text y="9" x="0" dy=".71em" style="text-anchor: middle;">
          2
        </text>
      </g>
      <g class="tick" transform="translate(510,0)" style="opacity: 1;">
        <line y2="6" x2="0"></line>
        <text y="9" x="0" dy=".71em" style="text-anchor: middle;">
          3
        </text>
      </g>
      <g class="tick" transform="translate(570,0)" style="opacity: 1;">
        <line y2="6" x2="0"></line>
        <text y="9" x="0" dy=".71em" style="text-anchor: middle;">
          4
        </text>
      </g>
      <g class="tick" transform="translate(630,0)" style="opacity: 1;">
        <line y2="6" x2="0"></line>
        <text y="9" x="0" dy=".71em" style="text-anchor: middle;">
          5
        </text>
      </g>
      <path
        fill="none"
        stroke="rgba(0, 0, 0, .1)"
        class="domain"
        d="M330,6V0H630V6"
      ></path>
    </g>
    <g
      class="axis"
      transform="translate(330,0)"
      style="font-size: 10px; pointer-events: none;"
    >
      <g class="tick" transform="translate(0,270)" style="opacity: 1;">
        <line x2="-6" y2="0"></line>
        <text x="-9" y="0" dy=".32em" style="text-anchor: end;">
          0
        </text>
      </g>
      <g class="tick" transform="translate(0,222)" style="opacity: 1;">
        <line x2="-6" y2="0"></line>
        <text x="-9" y="0" dy=".32em" style="text-anchor: end;">
          1
        </text>
      </g>
      <g class="tick" transform="translate(0,174)" style="opacity: 1;">
        <line x2="-6" y2="0"></line>
        <text x="-9" y="0" dy=".32em" style="text-anchor: end;">
          2
        </text>
      </g>
      <g
        class="tick"
        transform="translate(0,125.99999999999997)"
        style="opacity: 1;"
      >
        <line x2="-6" y2="0"></line>
        <text x="-9" y="0" dy=".32em" style="text-anchor: end;">
          3
        </text>
      </g>
      <g class="tick" transform="translate(0,78)" style="opacity: 1;">
        <line x2="-6" y2="0"></line>
        <text x="-9" y="0" dy=".32em" style="text-anchor: end;">
          4
        </text>
      </g>
      <g class="tick" transform="translate(0,30)" style="opacity: 1;">
        <line x2="-6" y2="0"></line>
        <text x="-9" y="0" dy=".32em" style="text-anchor: end;">
          5
        </text>
      </g>
      <path
        fill="none"
        stroke="rgba(0, 0, 0, .1)"
        class="domain"
        d="M-6,30H0V270H-6"
      ></path>
    </g>
  </g>
);

const vec = (x: number, y: number): [Var, Var] => [
  createMutable(variable(x)),
  createMutable(variable(y)),
];

export default () => {
  const v1 = vec(1, 0.5);
  const v2 = vec(0.5, 1);
  const v = vec(2, 3);
  const Av = ops.mvmul([v1, v2], v);
  const vd = v.map(numSignal);
  const avd = Av.map(numSignal);
  const v1d = v1.map(numSignal);
  const v2d = v2.map(numSignal);

  return (
    <div>
      <svg ref={svg} width={960} height={300}>
        <Axis />
        <g>
          <Vector id={"primary"} fill={"#3498db"} val={v1} label={"a₁"} />
          <Vector id={"secondary"} fill={"#2ecc71"} val={v2} label={"a₂"} />
          <Point fill={"rgb(231, 76, 60)"} val={v} label={"v"} draggable />
          <Point fill={"rgb(231, 76, 60)"} val={Av} label={"Av"} />
        </g>
      </svg>
      <br />
      v1: {v1d[0]()}, {v1d[1]()}
      <br />
      v2: {v2d[0]()}, {v2d[1]()}
      <br />
      v: {vd[0]()}, {vd[1]()}
      <br />
      Av: {avd[0]()}, {avd[1]()}
    </div>
  );
};
