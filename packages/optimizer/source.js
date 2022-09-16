import source from "./target/wasm32-unknown-unknown/release/penrose_optimizer.wasm";

export const getOptimizer = async () =>
  (await WebAssembly.instantiate(source)).instance.exports;
