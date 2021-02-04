import {
  attrCoords,
  attrFill,
  attrSide,
  attrSideCoords,
  attrStroke,
  attrTitle,
  attrWH,
} from "./AttrHelper";

// (old) COMBAK: Apparently our shapes don't deal with stroke opacity? All shapes should deal with stroke opacity

const Square = (shape: IShape, canvasSize: [number, number]) => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "rect");
  attrSideCoords(shape, canvasSize, elem);
  attrSide(shape, elem);
  attrFill(shape, elem);
  attrStroke(shape, elem);
  attrTitle(shape, elem);
  return elem;
};
export default Square;
