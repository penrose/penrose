import {
  attrCoords,
  attrFill,
  attrStroke,
  attrTitle,
  attrWH,
} from "./AttrHelper";

const Rectangle = (shape: IShape, canvasSize: [number, number]) => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "rect");
  attrCoords(shape, canvasSize, elem);
  attrWH(shape, elem);
  attrFill(shape, elem);
  attrStroke(shape, elem);
  attrTitle(shape, elem);
  return elem;
};
export default Rectangle;
