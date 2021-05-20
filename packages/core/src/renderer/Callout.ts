import { toScreen } from "utils/Util";
import {
  attrFill,
  attrRadiusX,
  attrRadiusY,
  attrStroke,
  attrTitle,
  attrWH,
  attrXY,
} from "./AttrHelper";
import { ShapeProps } from "./Renderer";

import * as v from "@thi.ng/vectors";

const makeCallout = (): [number, number][] => {
  const pts: [number, number][] = [
    [0, 0],
    [100, 100],
    [200, 0],
    [0, 0],
  ];

  const [z, x, y] = [[], [1, 2, 3, 4], [10, 20, 30, 40]]; // z is mutated; x and y are not
  const res = v.add(z, x, y); // also returns result
  const res2 = v.dist([1, 2], [100, 200]);

  console.log("vectors", res, res2);
  console.log("z, x, y", z, x, y);

  return pts;
};

const Callout = ({ shape, canvasSize }: ShapeProps) => {
  const elem = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "polygon"
  );
  attrFill(shape, elem);
  attrStroke(shape, elem);
  attrTitle(shape, elem);

  const pts = makeCallout();
  const ptsScreen = pts.map((p) => toScreen(p, canvasSize));
  elem.setAttribute("points", ptsScreen.toString());

  // attrPoints(shape, elem);

  // attrPolyCenter(shape, canvasSize, elem);
  // attrScale(shape, elem);

  return elem;

  // TODO: Change this from "rect"
  // const elem = document.createElementNS("http://www.w3.org/2000/svg", "rect");
  // attrXY(shape, canvasSize, elem);
  // attrWH(shape, elem);
  // attrFill(shape, elem);
  // attrStroke(shape, elem);
  // attrTitle(shape, elem);
  // attrRadiusX(shape, elem);
  // return elem;
};
export default Callout;
