import { Shape } from "types/shapes";
import { ShapeProps } from "./Renderer";

const Group = (
  { shape, canvasSize }: ShapeProps,
  nameShapeMap: { [k: string]: Shape }
): SVGGElement => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const subShapes = shape.properties["shapes"];
  if (subShapes.tag !== "ShapeListV") {
    throw Error("Not a list of shapes");
  } else {
    subShapes.contents.map();
  }
};
export default Group;
