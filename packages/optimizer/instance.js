import init from "./build/penrose_optimizer";
import bytes from "./build/penrose_optimizer_bg.wasm";

export default await init(bytes);
