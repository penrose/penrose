import init from "./build/penrose_optimizer";
import bytes from "./build/penrose_optimizer_bg.wasm";

export let maybeOptimizer = undefined;

export const optimizerReady = init(bytes).then((output) => {
  maybeOptimizer = output;
});
