import {
  Input,
  Num,
  add,
  cos,
  div,
  mul,
  numsOf,
  scalar,
  sin,
  sub,
} from "@penrose/core";
import seedrandom from "seedrandom";
import { createMutable } from "solid-js/store";

export interface TriangleProps {
  seed?: string;
  theta: Input;
}

export const Triangles = (props: TriangleProps) => {
  const [w, h] = [500, 500];
  const planeSize = 50;
  const planeHeight = -40;
  const cZ = -160;
  const rng = seedrandom(props.seed);

  const rotate = (vec: Num[], theta: Num) => [
    add(mul(vec[0], cos(theta)), mul(vec[2], sin(theta))),
    vec[1],
    sub(mul(vec[2], cos(theta)), mul(vec[0], sin(theta))),
  ];
  const perspective = (vec: Num[]) =>
    [vec[0], vec[1]].map((v) => mul(div(w, sub(vec[2], cZ)), v));
  const toCanvas = (p: number[]): number[] => [p[0] + w / 2, -p[1] + h / 2];

  // plane
  const plane = (theta: Input) => {
    const q00 = [-planeSize, planeHeight, -planeSize];
    const q10 = [planeSize, planeHeight, -planeSize];
    const q01 = [-planeSize, planeHeight, planeSize];
    const q11 = [planeSize, planeHeight, planeSize];

    const Qs = [q00, q10, q11, q01].map((v) => rotate(v, theta));

    const ps = Qs.map(perspective);
    const points = ps.map((p) => numsOf(p)).map(toCanvas);
    return (
      <polygon
        points={points.join(" ")}
        fill={"#0003"}
        stroke={"#aaa"}
        stroke-width={0.5}
      ></polygon>
    );
  };

  // triangles
  const triangleWithShadow = (
    qs: Input[][],
    theta: Input,
    fillColor: string
  ) => {
    // triangle
    const [qi, qj, qk] = qs.map((p) => rotate(p, theta));
    const ps = [qi, qj, qk].map(perspective);
    const triangle = ps.map((p) => numsOf(p)).map(toCanvas);
    const rs = [qi, qj, qk].map((p) => [p[0], planeHeight, p[2]]);
    const ss = rs.map(perspective);
    const shadow = ss.map((p) => numsOf(p)).map(toCanvas);
    return (
      <g>
        <polygon
          points={triangle.join(" ")}
          fill={fillColor}
          stroke={"#1b1f8a"}
          stroke-width={0.5}
        ></polygon>
        <polygon points={shadow.join(" ")} fill={"#0002"}></polygon>
      </g>
    );
  };

  const c = 0.9 * Math.min(planeSize, Math.abs(planeHeight));
  const inputs = (n: number) =>
    Array.from({ length: n }, () => scalar(-c + rng() * 2 * c));
  const tri1 = [inputs(3), inputs(3), inputs(3)];
  const tri2 = [inputs(3), inputs(3), inputs(3)];

  return (
    <svg
      version="1.2"
      xmlns="http://www.w3.org/2000/svg"
      width={800}
      height={800}
    >
      {plane(props.theta)}
      {triangleWithShadow(tri1, props.theta, "#34379a")}
      {triangleWithShadow(tri2, props.theta, "#340000")}
    </svg>
  );
};

export const RotatingTriangles = () => {
  const theta = createMutable<Input>(scalar(0));

  // the slider has to be defined inline so the entire slider is not re-rendered. Otherwise, the slider will allow continuous sliding because it gets re-rendered after each slide
  const onSlide = (n: number) => {
    theta.val = n;
  };

  return (
    <>
      <h1>Drag the slider to rotate the camera.</h1>
      <div>
        <div>
          Camera rotation
          <input
            type="range"
            min={0}
            max={10}
            step={0.1}
            value={theta.val}
            class="slider"
            onInput={(n) => onSlide(+n.target.value)}
            onChange={(n) => onSlide(+n.target.value)}
          />
          {theta.val}
        </div>
      </div>
      <Triangles theta={theta} />
    </>
  );
};
