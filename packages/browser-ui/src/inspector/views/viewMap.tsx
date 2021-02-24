import Frames from "./Frames";
import ShapeView from "./ShapeView";
import Mod from "./Mod";
import Errors from "inspector/views/Errors";
const viewMap = {
  frames: Frames,
  errors: Errors,
  // logs: LogView,
  shapes: ShapeView,
  mod: Mod
};
export default viewMap;
