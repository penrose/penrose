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
  problem,
  sub,
  textBBox,
  variable,
} from "@penrose/core";
import { greaterThan, inRange } from "@penrose/core/dist/contrib/Constraints";
import { toPt } from "@penrose/core/dist/contrib/Utils";
import { Num, Pt2 } from "@penrose/core/dist/types/ad";
import { createEffect, createResource, on } from "solid-js";
import { createMutable } from "solid-js/store";
import { num, signalNum } from "./util.js";

interface SetCircleProps {
  center: Var[];
  r: number;
}

interface SetsProps {
  sets: SetCircle[];
  labels: Label[];
  onFinish?: () => void;
}

const canvasWidth = 800;
const canvasHeight = 800;
let svg: SVGSVGElement; // this should be the reference to the circle
const ids = new Set(["s1-circle", "s2-circle", "s3-circle", "s1", "s2", "s3"]);

class Label {
  pt: Var[];
  private txt: string;
  private fontSize = "40px";
  private fontFamily = "STIXGeneral-Italic";

  constructor(x: number, y: number, txt: string) {
    this.pt = [createMutable(variable(x)), createMutable(variable(y))];
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
    return [
      recenter(textCorners.bottomRight),
      recenter(textCorners.bottomLeft),
      recenter(textCorners.topLeft),
      recenter(textCorners.topRight),
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
    // const [x, y] = toCanvas([num(pt[0]), num(pt[1])]);
    return (
      <g onMouseDown={(e) => onMouseDown(e, svg, this.pt)} id={`${this.txt}`}>
        {/* {this.getCorners().map((c) => this.renderPt(c))} */}
        {/* <circle
          cx={this.pt[0].val}
          cy={this.pt[1].val}
          r={3}
          fill="red"
        ></circle> */}
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
  constructor(cx: number, cy: number, r: number) {
    this.center = [createMutable(variable(cx)), createMutable(variable(cy))];
    // this.r = createMutable(variable(r));
    this.r = r;
  }

  getSet = (): SetCircleProps => {
    return {
      center: this.center,
      r: this.r,
    };
  };

  render = (fill: string, label: string) => {
    return (
      <g
        onMouseDown={(e) => onMouseDown(e, svg, this.center)}
        id={`${label}-circle`}
      >
        <circle
          cx={num(this.center[0])}
          cy={num(this.center[1])}
          r={this.r}
          fill={fill}
        ></circle>
      </g>
    );
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

//keep this
const onMouseDown = (e: MouseEvent, parent: SVGSVGElement, val: Var[]) => {
  const target = e.target as SVGSVGElement;
  console.log(target.innerHTML);
  const { clientX, clientY } = e;
  let { x: tempX, y: tempY } = getPosition({ clientX, clientY }, parent);
  let dx = 0,
    dy = 0;
  const onMouseMove = (e: MouseEvent) => {
    const { x, y } = getPosition(e, parent);
    dx = x - tempX;
    dy = y - tempY;
    tempX = x;
    tempY = y;
    // set the actual values
    val[0].val = val[0].val + dx;
    val[1].val = val[1].val + dy;
  };
  const onMouseUp = (e: MouseEvent) => {
    document.removeEventListener("mouseup", onMouseUp);
    document.removeEventListener("mousemove", onMouseMove);
    // remove transform
    ids.delete(target.id);
    target.setAttribute(`transform`, "");
    //trigger re-optimization
  };
  document.addEventListener("mouseup", onMouseUp);
  document.addEventListener("mousemove", onMouseMove);
};

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

  static onCanvas = (s: SetCircle, padding: number = 30) => {
    return [
      inRange(s.center[0], s.r + padding, canvasWidth - (s.r + padding)),
      inRange(s.center[1], s.r + padding, canvasHeight - (s.r + padding)),
    ];
    // const canvasCorners = [
    //   toPt([canvasWidth, 0]),
    //   toPt([0, 0]),
    //   toPt([0, canvasHeight]),
    //   toPt([canvasWidth, canvasHeight]),
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
    const minDist = Math.max(s1.r, s2.r);
    return greaterThan(
      dist(toNumArr(s2.center), toNumArr(s1.center)),
      minDist,
      padding,
    );
  };
}

export const Sets = (props: SetsProps) => {
  const s1color = "#3498db";
  const s2color = "#ab91eb";
  const s3color = "#cbbaf5";
  const [s1, s2, s3] = props.sets;
  const [l1, l2, l3] = props.labels;

  // subsets
  const s1containss2 = Constraints.isSubset(s1, s2);
  const s1containss3 = Constraints.isSubset(s1, s3);

  // labels
  const s1l1 = Constraints.containsText(s1, l1);
  const s2l2 = Constraints.containsText(s2, l2);
  const s3l3 = Constraints.containsText(s3, l3);

  const s1onCanvas = Constraints.onCanvas(s1);
  // const s2onCanvas = Constraints.onCanvas(s2);
  // const s3onCanvas = Constraints.onCanvas(s3);

  const s2s3disjoint = Constraints.disjointCircles(s2, s3);

  const waiting: Promise<void>[] = [];
  const [compiling] = createResource(() => {
    const prob = problem({
      constraints: [
        s1containss2,
        s1containss3,
        s1onCanvas[0],
        s1onCanvas[1],
        s2s3disjoint,
        // s2onCanvas[0],
        // s2onCanvas[1],
        // s3onCanvas[0],
        // s3onCanvas[1],
        s1l1,
        s2l2,
        s3l3,
      ],
    });
    waiting.push(prob.then(() => {}));
    return prob;
  });

  // TODO need to create effect to RUN the optimization for something related to labels here?
  const centers = () => props.sets;
  createEffect(
    on([centers, compiling], ([ps, prob]) => {
      // TODO need this to be triggered by mouseup and onload conditions
      if (prob !== undefined) {
        // TODO need a map between id and penrose object for freeze? :\
        // const run = prob.start({ freeze: (x) => !ids.has(x) }).run({});
        const run = prob.start({}).run({});
        console.log(run);
        for (const [v, x] of run.vals) v.val = x;
      }
    }),
  );

  createEffect(async () => {
    const f = props.onFinish;
    if (f) {
      await Promise.all(waiting);
      f();
    }
  });

  return (
    <div style={{ display: "flex", "align-items": "center" }}>
      <svg
        ref={svg}
        width={canvasWidth}
        height={canvasHeight}
        style={{ "background-color": "lightgray" }}
      >
        <g>
          {s1.render(s1color, "s1")}
          {s2.render(s2color, "s2")}
          {s3.render(s3color, "s3")}
          {l1.render()}
          {l2.render()}
          {l3.render()}
        </g>
      </svg>
    </div>
  );
};

export default () => {
  const s1 = new SetCircle(
    Math.random() * canvasWidth,
    Math.random() * canvasHeight,
    300,
  );
  const s2 = new SetCircle(
    Math.random() * canvasWidth,
    Math.random() * canvasHeight,
    100,
  );
  const s3 = new SetCircle(230, 200, 100);
  const l1 = new Label(
    Math.random() * canvasWidth,
    Math.random() * canvasHeight,
    "s1",
  );
  const l2 = new Label(
    Math.random() * canvasWidth,
    Math.random() * canvasHeight,
    "s222",
  );
  const l3 = new Label(300, 300, "s3");

  return (
    <div
      style={{ display: "flex", width: "100%", "justify-content": "center" }}
    >
      <div style={{ "font-family": "Lato", margin: "0 150px" }}>
        <Sets sets={[s1, s2, s3]} labels={[l1, l2, l3]} />
      </div>
    </div>
  );
};
