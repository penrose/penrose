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

const makeCallout = (): [number, number][] => {
  const pts: [number, number][] = [
    [0, 0],
    [100, 100],
    [200, 0],
    [0, 0],
  ];
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
