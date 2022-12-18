import init from "./build/penrose_optimizer";
// the reason we make this file JavaScript with a separate `instance.d.ts` file,
// instead of just making this file TypeScript directly, is because we customize
// the loader esbuild uses for the Wasm file, and TypeScript wouldn't understand
// this `import`
import bytes from "./build/penrose_optimizer_bg.wasm";

// we let this be initialized sometime after module load, because our usage of
// webpack for `docs-site` prevents us from using top-level `await` here
export let maybeOptimizer = undefined;

// we pass `--target=web` to `wasm-bindgen`, because we need control over the
// way the WebAssembly module is initialized; and we pass
// `--define:import.meta.url=null` to esbuild, because in web mode,
// `wasm-bindgen` generates a default case for `init` that uses a `new URL` with
// `import.meta.url`, which trips up Vite after we bundle this into a file in a
// directory that doesn't contain the original Wasm file; but since we pass the
// bytes directly to `init`, we don't need that default case, so we just tell
// esbuild to replace `import.meta.url` with something that doesn't trigger
// Vite's special asset handling
export const optimizerReady = init(bytes).then((output) => {
  maybeOptimizer = output;
});
