import ArrowDef from "./shapedefs/arrow.json";
import CircDef from "./shapedefs/circle.json";
import CurveDef from "./shapedefs/curve.json";
import EllipseDef from "./shapedefs/ellipse.json";
import ImageDef from "./shapedefs/image.json";
import LineDef from "./shapedefs/line.json";
import PathStrDef from "./shapedefs/pathStr.json";
import {
  default as FreeformPolygonDef,
  default as PolygonDef,
} from "./shapedefs/polygon.json";
import PolylineDef from "./shapedefs/polyline.json";
import RectangleDef from "./shapedefs/rectangle.json";
import SquareDef from "./shapedefs/square.json";
import TextDef from "./shapedefs/text.json";

const defMap = {
  Circle: CircDef,
  Line: LineDef,
  Ellipse: EllipseDef,
  Square: SquareDef,
  Rectangle: RectangleDef,
  Polygon: PolygonDef,
  FreeformPolygon: FreeformPolygonDef,
  Polyline: PolylineDef,
  Arrow: ArrowDef,
  Image: ImageDef,
  Text: TextDef,
  Path: CurveDef,
  PathString: PathStrDef,
};
export default defMap;
