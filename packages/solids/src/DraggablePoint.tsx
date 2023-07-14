import { Var } from "@penrose/core";
import { num, signalNum } from "./util.js";
/**
 * Converts screen to relative SVG coords
 * Thanks to
 * https://www.petercollingridge.co.uk/tutorials/svg/interactive/dragging/
 * @param e
 * @param svg
 */
const getPosition = (
  { clientX, clientY }: { clientX: number; clientY: number },
  svg?: SVGSVGElement
) => {
  const CTM = svg?.getScreenCTM();
  if (CTM && CTM !== null) {
    return { x: (clientX - CTM.e) / CTM.a, y: (clientY - CTM.f) / CTM.d };
  }
  return { x: 0, y: 0 };
};

const onMouseDown = (
  e: MouseEvent,
  parent: SVGSVGElement | undefined,
  val: Var[],
  transform: {
    toScreen: (xy: number[]) => number[];
    toModel: (xy: number[]) => number[];
  },
  constrain: (xy: number[]) => number[]
) => {
  const target = e.target as SVGSVGElement;
  // avoid highlighting text etc.
  e.preventDefault();
  target.setAttribute("fill-opacity", "0.15");
  const { clientX, clientY } = e;
  let { x: tempX, y: tempY } = getPosition({ clientX, clientY }, parent);
  let lastX = val[0].val;
  let lastY = val[1].val;
  let dx = 0,
    dy = 0;
  const onMouseMove = (e: MouseEvent) => {
    const { x, y } = getPosition(e, parent);
    dx = x - tempX;
    dy = tempY - y;
    // set the actual values
    const [mx, my] = transform.toModel([dx, dy]);

    const futureX = val[0].val + mx;
    const futureY = val[1].val + my;
    const [nextX, nextY] = constrain([futureX, futureY]);
    val[0].val = nextX;
    val[1].val = nextY;

    // TODO: only update when the value update will happen
    if (nextX !== lastX) {
      tempX = x;
      lastX = nextX;
    }
    if (nextY !== lastY) {
      tempY = y;
      lastY = nextY;
    }
  };
  const onMouseUp = (e: MouseEvent) => {
    document.removeEventListener("mouseup", onMouseUp);
    document.removeEventListener("mousemove", onMouseMove);
    target.setAttribute("fill-opacity", "0.1");
  };
  document.addEventListener("mouseup", onMouseUp);
  document.addEventListener("mousemove", onMouseMove);
};

export default ({
  x,
  y,
  svg,
  constrain,
  transform,
}: {
  x: Var;
  y: Var;
  svg?: SVGSVGElement; // containing SVG element for computing the correct transformation
  transform: {
    toScreen: (xy: number[]) => number[];
    toModel: (xy: number[]) => number[];
  };
  constrain: (xy: number[]) => number[];
}) => {
  const [xd, yd] = [x, y].map(signalNum);
  return (
    <g onMouseDown={(e) => onMouseDown(e, svg, [x, y], transform, constrain)}>
      <circle
        cx={transform.toScreen([num(xd), num(yd)])[0]}
        cy={transform.toScreen([num(xd), num(yd)])[1]}
        r="22.91"
        fill="#000"
        fill-opacity={0.1}
      ></circle>
    </g>
  );
};
