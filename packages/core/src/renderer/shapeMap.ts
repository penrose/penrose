import Circle from "./Circle";
import Ellipse from "./Ellipse";
import Equation from "./Equation";
import Image from "./Image";
import Line from "./Line";
import Path from "./Path";
import Polygon from "./Polygon";
import Polyline from "./Polyline";
import Rectangle from "./Rectangle";
import { ShapeProps } from "./Renderer";
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
