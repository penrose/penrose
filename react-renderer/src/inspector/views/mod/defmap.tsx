import CircDef from "./shapedefs/circle.json"
import LineDef from "./shapedefs/line.json"
import EllipseDef from "./shapedefs/ellipse.json"
import SquareDef from "./shapedefs/square.json"
import RectangleDef from "./shapedefs/rectangle.json"
import ArrowDef from "./shapedefs/arrow.json"
import ImageDef from "./shapedefs/image.json"
import TextDef from "./shapedefs/text.json"
const defMap = {
  Circle: CircDef,
  Line: LineDef,
  Ellipse: EllipseDef,
  Square: SquareDef,
  Rectangle: RectangleDef,
  Arrow: ArrowDef,
  Image: ImageDef,
  Text: TextDef
};
export default defMap;
