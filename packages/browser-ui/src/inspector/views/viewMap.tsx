//import Opt from "./Opt";
import Errors from "../../inspector/views/Errors";
import CompGraph from "./CompGraph";
import Frames from "./Frames";
import Opt from "./Opt";
import Settings from "./Settings";
import ShapeView from "./ShapeView";

const viewMap = {
  state: Frames,
  errors: Errors,
  shapes: ShapeView,
  // mod: Mod, // NOTE: mod tab temporarily deprecated
  "optimization status": Opt,
  compGraph: CompGraph,
  settings: Settings,
};

export default viewMap;
