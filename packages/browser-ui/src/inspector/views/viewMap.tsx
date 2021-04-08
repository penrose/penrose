import Frames from "./Frames";
import ShapeView from "./ShapeView";
import Mod from "./Mod";
import Errors from "inspector/views/Errors";
import Opt from "./Opt";
import CompGraph from "./CompGraph";
const viewMap = {
  frames: Frames,
  errors: Errors,
  // logs: LogView,
  shapes: ShapeView,
  mod: Mod,
  opt: Opt,
  compGraph: CompGraph,
};
export default viewMap;
