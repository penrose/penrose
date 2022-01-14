import Circle from "./Circle";
import Equation from "./Label";
import Ellipse from "./Ellipse";
import Rectangle from "./Rectangle";
import Polygon from "./Polygon";
import Polyline from "./Polyline";
import { ShapeProps } from "./Renderer";
import Path from "./Path";
import Line from "./Line";
import Image from "./Image";
import Text from "./Text";

const shapeMap: {
  [key: string]: (props: ShapeProps) => Promise<SVGElement> | SVGElement;
} = {
  Circle,
  Ellipse,
  Rectangle,
  Polygon,
  Polyline,
  Equation,
  Path,
  Line,
  Image,
  Text,
};

export default shapeMap;
