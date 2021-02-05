import Circle from "./Circle";
import Label from "./Label";
import Ellipse from "./Ellipse";
import Square from "./Square";
import Rectangle from "./Rectangle";
import Arrow from "./Arrow";
import { ShapeProps } from "./Renderer";
import Path from "./Path";

const shapeMap: { [key: string]: (props: ShapeProps) => SVGElement } = {
  Circle,
  Ellipse,
  Square,
  Rectangle,
  Text: Label,
  Arrow,
  Path,
};

export default shapeMap;
