import Frames from "./Frames";
import ShapeView from "./ShapeView";
import Mod from "./Mod";
import Errors from "inspector/views/Errors";
import Opt from "./Opt";
import CompGraph from "./CompGraph";
import Settings from "./Settings";
import Jacobian from "./Jacobian";
const viewMap = {
  frames: Frames,
  errors: Errors,
  shapes: ShapeView,
  mod: Mod,
  "optimization status": Opt,
  compGraph: CompGraph,
  settings: Settings,
  jacobian: Jacobian,
};

export default viewMap;
