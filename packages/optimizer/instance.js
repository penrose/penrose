import { initSync } from "./build/penrose_optimizer";
import bytes from "./build/penrose_optimizer_bg.wasm";

export default initSync(bytes);
