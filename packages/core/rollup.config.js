import commonjs from "@rollup/plugin-commonjs";
import json from "@rollup/plugin-json";
import { nodeResolve } from "@rollup/plugin-node-resolve";
import nodePolyfills from "rollup-plugin-node-polyfills";
import typescript from "rollup-plugin-typescript2";
import pkg from "./package.json";

const input = "./src/index.ts";
const plugins = [
  typescript({
    tsconfig: "tsconfig.json",
  }),
  nodeResolve({
    preferBuiltins: false,
    browser: true,
  }),
  commonjs(),
  nodePolyfills(),
  json(),
];

const onwarn = (warning, defaultHandler) => {
  if (warning.code !== "CIRCULAR_DEPENDENCY") {
    defaultHandler(warning);
  }
};

export default [
  {
    input,
    output: {
      file: pkg.unpkg,
      name: "penrose", // this is the name of the global object
      esModule: false,
      exports: "named",
      format: "umd",
      sourcemap: true,
    },
    onwarn,
    watch: false, // no need to rebuild during watch
    plugins: [
      ...plugins,
      // terser({ format: { comments: false } }),
      // visualizer()
    ],
  },
  // DEPRECATED: ESM and CJS builds are supported by esbuild now
  // {
  //   input,
  //   output: {
  //     file: pkg.module,
  //     format: "esm",
  //     sourcemap: true,
  //   },
  //   onwarn,
  //   watch: false, // no need to rebuild during watch - targeting node only
  //   plugins,
  //   cache: pkg.module, // for incremental builds
  // },
  // {
  //   input,
  //   output: {
  //     file: pkg.main,
  //     format: "cjs",
  //     sourcemap: true,
  //   },
  //   watch: true, // no need to rebuild during watch - targeting node only
  //   onwarn,
  //   plugins,
  //   cache: pkg.main, // for incremental builds
  // },
];
