import Frames from "./Frames";
import ShapeView from "./ShapeView";
import Mod from "./Mod";
import Errors from "inspector/views/Errors";
import Opt from "./Opt";
import CompGraph from "./CompGraph";
import Settings from "./Settings";
import Palette from "./Palette";
const viewMap = {
  frames: Frames,
  errors: Errors,
  shapes: ShapeView,
  mod: Mod,
  "palette": Palette,
  "optimization status": Opt,
  compGraph: CompGraph,
  settings: Settings,
};

export default viewMap;
