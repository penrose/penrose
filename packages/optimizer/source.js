import source from "./optimizer.wasm";

export const answer = async () => {
  const { instance } = await WebAssembly.instantiate(source);
  return () => instance.exports.answer();
};
