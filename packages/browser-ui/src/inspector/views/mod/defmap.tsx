import CircDef from "./shapedefs/circle.json";
import LineDef from "./shapedefs/line.json";
import EllipseDef from "./shapedefs/ellipse.json";
import SquareDef from "./shapedefs/square.json";
import RectangleDef from "./shapedefs/rectangle.json";
import PolygonDef from "./shapedefs/polygon.json";
import FreeformPolygonDef from "./shapedefs/polygon.json";
import PolylineDef from "./shapedefs/polyline.json";
import ArrowDef from "./shapedefs/arrow.json";
import ImageDef from "./shapedefs/image.json";
import TextDef from "./shapedefs/text.json";
import CurveDef from "./shapedefs/curve.json";

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
};
export default defMap;
