import typescript from "rollup-plugin-typescript2";
import visualizer from "rollup-plugin-visualizer";
import { terser } from "rollup-plugin-terser";
import nodePolyfills from "rollup-plugin-node-polyfills";
import { nodeResolve } from "@rollup/plugin-node-resolve";
import commonjs from "@rollup/plugin-commonjs";
import json from "@rollup/plugin-json";
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
      file: pkg.bundle,
      name: "penrose", // this is the name of the global object
      esModule: false,
      exports: "named",
      format: "umd",
      sourcemap: true,
    },
    onwarn,
    plugins: [...plugins, terser(), visualizer()],
  },
  {
    input,
    output: {
      file: pkg.module,
      format: "esm",
      sourcemap: true,
    },
    onwarn,
    plugins,
  },
  {
    input,
    output: {
      file: pkg.main,
      format: "cjs",
      sourcemap: true,
    },
    onwarn,
    plugins,
  },
];
