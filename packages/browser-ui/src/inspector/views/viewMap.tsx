import Frames from "./Frames";
import ShapeView from "./ShapeView";
import Errors from "inspector/views/Errors";
import Opt from "./Opt";
import CompGraph from "./CompGraph";
import Settings from "./Settings";

const viewMap = {
  state: Frames,
  errors: Errors,
  shapes: ShapeView,
  // mod: Mod, // NOTE: mod tab temporarily deprecated
  //"optimization status": Opt,
  compGraph: CompGraph,
  settings: Settings,
};

export default viewMap;
