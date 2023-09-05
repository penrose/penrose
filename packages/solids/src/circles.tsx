import {
  BBox,
  Var,
  containsCircleRect,
  containsCircles,
  containsRectCircle,
  corners,
  measureText,
  problem,
  textBBox,
  variable,
} from "@penrose/core";
import { inRange } from "@penrose/core/dist/contrib/Constraints";
import { toPt } from "@penrose/core/dist/contrib/Utils";
import { Num, Pt2 } from "@penrose/core/dist/types/ad";
import { createEffect, createResource, on } from "solid-js";
import { createMutable } from "solid-js/store";
import { num } from "./util.js";

interface Rect {
  center: Num[];
  width: Num;
  height: Num;
}

interface SetProps {
  center: Var[];
  r: number;
}

interface SetsProps {
  sets: Set[];
  labels: Label[];
  onFinish?: () => void;
}

const canvasWidth = 800;
const canvasHeight = 800;
let svg: SVGSVGElement; // this should be the reference to the circle

class Label {
  pt: Var[];
  private txt: string;
  private fontSize = "30px";
  private fontFamily = "STIXGeneral-Italic";

  constructor(x: number, y: number, txt: string) {
    this.pt = [createMutable(variable(x)), createMutable(variable(y))];
    this.txt = txt;
  }

  getCorners = (): Pt2[] => {
    const bbox = textBBox(
      measureText(this.txt, `${this.fontSize} "${this.fontFamily}", `),
      this.pt[0],
      this.pt[1],
    );
    const textCorners = corners(bbox as BBox);
    return [
      textCorners.topRight,
      textCorners.topLeft,
      textCorners.bottomLeft,
      textCorners.bottomRight,
    ]; // TODO order? should it be same as the canvas order?
  };

  render = () => {
    // const [x, y] = toCanvas([num(pt[0]), num(pt[1])]);
    return (
      <g onMouseDown={(e) => onMouseDown(e, svg, this.pt)}>
        <circle
          cx={this.pt[0].val}
          cy={this.pt[1].val}
          r={3}
          fill="red"
        ></circle>
        <text
          x={num(this.pt[0])}
          y={num(this.pt[1])}
          fill-color="black"
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

class Set {
  center: Var[];
  r: number;
  constructor(cx: number, cy: number, r: number) {
    this.center = [createMutable(variable(cx)), createMutable(variable(cy))];
    // this.r = createMutable(variable(r));
    this.r = r;
  }

  getSet = (): SetProps => {
    return {
      center: this.center,
      r: this.r,
    };
  };

  render = (fill: string, label: string) => {
    return (
      <g onMouseDown={(e) => onMouseDown(e, svg, this.center)}>
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
    // console.log(x, y, dx);
    val[0].val = val[0].val + dx;
    val[1].val = val[1].val + dy;
  };
  const onMouseUp = (e: MouseEvent) => {
    document.removeEventListener("mouseup", onMouseUp);
    document.removeEventListener("mousemove", onMouseMove);
    // remove transform
    target.setAttribute(`transform`, "");
    //trigger re-optimization
  };
  document.addEventListener("mouseup", onMouseUp);
  document.addEventListener("mousemove", onMouseMove);
};

class Constraints {
  static isSubset = (s1: Set, s2: Set, padding: number = 10) => {
    return containsCircles(
      toPt(s1.center),
      s1.r,
      toPt(s2.center),
      s2.r,
      padding,
    );
  };

  static onCanvas = (s: Set, canvas: Pt2[], padding: number = 30) => {
    return containsRectCircle(canvas, toPt(s.center), num(s.r), num(padding));
  };

  static containsText = (s: Set, label: Label, padding: number = 50) => {
    return containsCircleRect(toPt(s.center), s.r, label.getCorners(), padding);
  };
}

export const Sets = (props: SetsProps) => {
  const a1Color = "#3498db";
  const a2Color = "#2ecc71";
  const vColor = "#E74C3C";
  const [s1, s2, s3] = props.sets;
  const [l1, l2, l3] = props.labels;

  // subsets
  const s1containss2 = Constraints.isSubset(s1, s2);
  const s1containss3 = Constraints.isSubset(s1, s3);

  // labels
  const s1l1 = Constraints.containsText(s1, l1);
  const s2l2 = Constraints.containsText(s2, l2);
  const s3l3 = Constraints.containsText(s3, l3);

  const onCanvas = (set: Set) => {
    const canvasCorners = [
      toPt([canvasWidth, 0]),
      toPt([0, 0]),
      toPt([0, canvasHeight]),
      toPt([canvasWidth, canvasHeight]),
    ];
    return Constraints.onCanvas(set, canvasCorners);
  };

  const waiting: Promise<void>[] = [];
  const [compiling] = createResource(() => {
    const prob = problem({
      constraints: [
        inRange(s1.center[0], 300, 500),
        inRange(s1.center[1], 300, 500),
        s1containss2,
        s1containss3,
        // onCanvas(s1),
        // onCanvas(s2),
        // onCanvas(s3),
        s1l1,
        s2l2,
        s3l3,
      ],
    });
    waiting.push(prob.then(() => {}));
    return prob;
  });

  // TODO need to create effect to RUN the optimization for something related to labels here?
  // const centers = () => props.sets.map((pts) => pts.map((pt) => num(pt)));
  const centers = () => props.sets;
  createEffect(
    on([centers, compiling], ([ps, prob]) => {
      // TODO something probably wrong here with centers, at least needs to include labels?
      // ps.forEach((p, i) => {
      //   props.sets[i][0].val = p[0];
      //   props.sets[i][1].val = p[1];
      // });
      if (prob !== undefined) {
        // const run = prob.start({ freeze: (x) => !labelSet.has(x) }).run({});
        const run = prob.start({}).run({});
        // console.log(run);
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
        style={{ "background-color": "purple" }}
      >
        <g>
          {s1.render(a1Color, "s1")}
          {s2.render(a2Color, "s2")}
          {s3.render(vColor, "s3")}
          {l1.render()}
          {l2.render()}
          {l3.render()}
        </g>
      </svg>
    </div>
  );
};

export default () => {
  const s1 = new Set(
    Math.random() * canvasWidth,
    Math.random() * canvasHeight,
    300,
  );
  const s2 = new Set(
    Math.random() * canvasWidth,
    Math.random() * canvasHeight,
    100,
  );
  const s3 = new Set(230, 200, 100);
  const l1 = new Label(
    Math.random() * canvasWidth,
    Math.random() * canvasHeight,
    "s1",
  );
  const l2 = new Label(
    Math.random() * canvasWidth,
    Math.random() * canvasHeight,
    "s2",
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
