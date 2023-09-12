import {
  BBox,
  Var,
  add,
  containsCircleRect,
  containsCircles,
  corners,
  dist,
  div,
  measureText,
  neg,
  problem,
  shapeDistanceRects,
  sub,
  textBBox,
  variable,
} from "@penrose/core";
import { greaterThanSq, inRange } from "@penrose/core/dist/contrib/Constraints";
import { toPt } from "@penrose/core/dist/contrib/Utils";
import { Num, Problem, Pt2 } from "@penrose/core/dist/types/ad";
import { noWarn } from "@penrose/core/dist/utils/Util";
import { createEffect, createResource, on } from "solid-js";
import { createMutable } from "solid-js/store";
import { num, signalNum } from "./util.js";

// TODO constraints vs encourages? disjoint is working ehhh, no label overlap
// need some help writing an energy function for "encourage" and messing with constraint graph when an object is moved to a place where some constraints can no longer be satisfied
// without removing constraints?
// api feedback: would be nice to have ability to call the high-level constraint methods like disjoint, overlapping, etc.
// instead of the helper functions.

const [CANVAS_WIDTH, CANVAS_HEIGHT] = [800, 800];
const frozen = new Set();
let svg: SVGSVGElement;
let prob: Problem | undefined;

class Label {
  pt: Var[];
  private txt: string;
  private fontSize = "40px";
  private fontFamily = "STIXGeneral-Italic";

  constructor(txt: string) {
    this.pt = [
      createMutable(variable(Math.random() * CANVAS_WIDTH)),
      createMutable(variable(Math.random() * CANVAS_HEIGHT)),
    ];
    this.txt = txt;
  }

  getCorners = (): Pt2[] => {
    const bbox = textBBox(
      measureText(this.txt, `${this.fontSize} "${this.fontFamily}"`),
      this.pt[0],
      this.pt[1],
    );
    const textCorners = corners(bbox as BBox);
    // move bbox up and right.. with variable results when plotted?
    const recenter = (pt: Pt2) =>
      [add(pt[0], div(bbox.width, 2)), sub(pt[1], bbox.height)] as Pt2;
    // return [
    //   recenter(textCorners.bottomRight),
    //   recenter(textCorners.bottomLeft),
    //   recenter(textCorners.topLeft),
    //   recenter(textCorners.topRight),
    // ];
    return [
      textCorners.bottomRight,
      textCorners.bottomLeft,
      textCorners.topLeft,
      textCorners.topRight,
    ];
  };

  private renderPt = (pt: Num[]) => {
    return (
      <circle
        cx={num(signalNum(pt[0]))}
        cy={num(signalNum(pt[1]))}
        r={3}
        fill="yellow"
      ></circle>
    );
  };

  render = () => {
    return (
      <g onMouseDown={(e) => onMouseDown(e, svg, this.pt)} id={`${this.txt}`}>
        {this.getCorners().map((c) => this.renderPt(c))}
        <circle
          cx={this.pt[0].val}
          cy={this.pt[1].val}
          r={3}
          fill="red"
        ></circle>
        <text
          x={num(this.pt[0])}
          y={num(this.pt[1])}
          fill-color="white"
          font-style="italic"
          font-weight="bold"
          font-family={this.fontFamily}
          stroke="white"
          stroke-width={3}
          paint-order="stroke"
          font-size={this.fontSize}
        >
          {this.txt}
        </text>
      </g>
    );
  };
}

class SetCircle {
  center: Var[];
  r: number;
  color: string;
  label: string;
  constructor(r: number, color: string, label: string) {
    this.center = [
      createMutable(variable(Math.random() * CANVAS_WIDTH)),
      createMutable(variable(Math.random() * CANVAS_HEIGHT)),
    ];
    // this.r = createMutable(variable(r));
    this.r = r;
    this.color = color;
    this.label = label;
  }

  render = () => {
    return (
      <g
        onMouseDown={(e) => onMouseDown(e, svg, this.center)}
        id={`${this.label}-circle`}
      >
        <circle
          cx={num(this.center[0])}
          cy={num(this.center[1])}
          r={this.r}
          fill={this.color}
        ></circle>
      </g>
    );
  };
}

class Constraints {
  static isSubset = (s1: SetCircle, s2: SetCircle, padding: number = 10) => {
    return containsCircles(
      toPt(s1.center),
      s1.r,
      toPt(s2.center),
      s2.r,
      padding,
    );
  };

  static disjointLabels = (l1: Label, l2: Label, padding: number = 50) => {
    const { value: dist, warnings } = noWarn(
      // what warning am i suppressing here?
      shapeDistanceRects(l1.getCorners(), l2.getCorners()),
    );
    return greaterThanSq(dist, add(dist, neg(padding)));
  };

  static onCanvas = (s: SetCircle, padding: number = 30) => {
    return [
      inRange(s.center[0], s.r + padding, CANVAS_WIDTH - (s.r + padding)),
      inRange(s.center[1], s.r + padding, CANVAS_HEIGHT - (s.r + padding)),
    ];
    // const canvasCorners = [
    //   toPt([CANVAS_WIDTH, 0]),
    //   toPt([0, 0]),
    //   toPt([0, CANVAS_HEIGHT]),
    //   toPt([CANVAS_WIDTH, CANVAS_HEIGHT]),
    // ];
    // return containsRectCircle(canvas, toPt(s.center), num(s.r), num(padding));
  };

  static containsText = (s: SetCircle, label: Label, padding: number = 50) => {
    return containsCircleRect(toPt(s.center), s.r, label.getCorners(), padding);
  };

  static disjointCircles = (
    s1: SetCircle,
    s2: SetCircle,
    padding: number = 30,
  ) => {
    const toNumArr = (c: Var[]) => c.map((v) => v.val);
    const minDist = s1.r + s2.r + padding;
    return greaterThanSq(
      dist(toNumArr(s2.center), toNumArr(s1.center)),
      minDist,
    );
  };

  static build = (sets: SetCircle[], labels: Label[]) => {
    const [s1, s2, s3] = sets;
    const [l1, l2, l3] = labels;
    // subsets
    const s1containss2 = Constraints.isSubset(s1, s2);
    const s1containss3 = Constraints.isSubset(s1, s3);

    // labels
    const s1l1 = Constraints.containsText(s1, l1);
    const s2l2 = Constraints.containsText(s2, l2);
    const s3l3 = Constraints.containsText(s3, l3);
    const disl1l2 = Constraints.disjointLabels(l1, l2);
    const disl1l3 = Constraints.disjointLabels(l1, l3);
    const disl2l3 = Constraints.disjointLabels(l2, l3);

    const s1onCanvas = Constraints.onCanvas(s1);

    const s2s3disjoint = Constraints.disjointCircles(s2, s3);
    return [
      s1containss2,
      s1containss3,
      s1onCanvas[0],
      s1onCanvas[1],
      s2s3disjoint,
      s1l1,
      s2l2,
      s3l3,
      disl1l2,
      disl1l3,
      disl2l3,
    ];
  };
}

/**
 * Converts screen to relative SVG coords
 * Thanks to
 * https://www.petercollingridge.co.uk/tutorials/svg/interactive/dragging/
 * @param e
 * @param svg
 */
const getPosition = (
  { clientX, clientY }: { clientX: number; clientY: number },
  svg: SVGSVGElement,
) => {
  const CTM = svg.getScreenCTM();
  if (CTM !== null) {
    return { x: (clientX - CTM.e) / CTM.a, y: (clientY - CTM.f) / CTM.d };
  }
  return { x: 0, y: 0 };
};

const onMouseDown = (e: MouseEvent, parent: SVGSVGElement, val: Var[]) => {
  const { clientX, clientY } = e;
  let { x: tempX, y: tempY } = getPosition({ clientX, clientY }, parent);
  let [dx, dy] = [0, 0];
  const onMouseMove = (e: MouseEvent) => {
    const { x, y } = getPosition(e, parent);
    [dx, dy] = [x - tempX, y - tempY];
    [tempX, tempY] = [x, y];
    // set the actual values
    val[0].val = val[0].val + dx;
    val[1].val = val[1].val + dy;
  };
  const onMouseUp = (e: MouseEvent) => {
    document.removeEventListener("mouseup", onMouseUp);
    document.removeEventListener("mousemove", onMouseMove);

    // add x, y variables to freeze list for resample
    val.forEach((x) => frozen.add(x));
    //trigger re-optimization
    resample();
    val.forEach((x) => frozen.delete(x));
  };
  document.addEventListener("mouseup", onMouseUp);
  document.addEventListener("mousemove", onMouseMove);
};

const resample = () => {
  if (prob !== undefined) {
    const run = prob.start({ freeze: (x) => frozen.has(x) }).run({});
    console.log(run, frozen);
    for (const [v, x] of run.vals) v.val = x;
  }
};

export default () => {
  const sets = [
    new SetCircle(300, "#3498db", "s1"),
    new SetCircle(100, "#ab91eb", "s2"),
    new SetCircle(100, "#cbbaf5", "s3"),
  ];
  const labels = [new Label("s1"), new Label("s222"), new Label("s3")];
  const constraints = Constraints.build(sets, labels);

  const [compiling] = createResource(() => {
    const prob = problem({
      constraints: constraints,
    });
    prob.then(() => {});
    return prob;
  });

  createEffect(
    on(compiling, (p) => {
      prob = p;
      resample();
    }),
  );

  return (
    <div
      style={{ display: "flex", width: "100%", "justify-content": "center" }}
    >
      <div style={{ "font-family": "Lato", margin: "0 150px" }}>
        <div style={{ display: "flex", "align-items": "center" }}>
          <svg
            ref={svg}
            width={CANVAS_WIDTH}
            height={CANVAS_HEIGHT}
            style={{ "background-color": "lightgray" }}
          >
            <g>
              {sets.map((set) => set.render())}
              {labels.map((label) => label.render())}
            </g>
          </svg>
        </div>
      </div>
    </div>
  );
};
