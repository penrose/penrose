import Frames from "./Frames";
import ShapeView from "./ShapeView";
import Mod from "./Mod";
import Errors from "inspector/views/Errors";
import Opt from "./Opt";
const viewMap = {
  frames: Frames,
  errors: Errors,
  // logs: LogView,
  shapes: ShapeView,
  mod: Mod,
  opt: Opt
};
export default viewMap;
