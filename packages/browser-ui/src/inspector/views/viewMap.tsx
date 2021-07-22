import Frames from "./Frames";
import ShapeView from "./ShapeView";
import Mod from "./Mod";
import Errors from "inspector/views/Errors";
import Opt from "./Opt";
import Settings from "./Settings";
import Jacobian from "inspector/views/Jacobian";
const viewMap = {
  frames: Frames,
  errors: Errors,
  shapes: ShapeView,
  mod: Mod,
  "optimization status": Opt,
  settings: Settings,
  jacobian: Jacobian,
};
export default viewMap;
