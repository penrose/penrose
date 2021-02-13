import typescript from "rollup-plugin-typescript2";
import replace from "@rollup/plugin-replace";
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
  nodeResolve(),
  commonjs(),
  json(),
];
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
    plugins: [
      typescript({
        tsconfig: "tsconfig.json",
      }),
      nodeResolve({
        browser: true,
      }),
      commonjs(),
      json(),
      nodePolyfills(),
      replace({
        "process.env.NODE_ENV": JSON.stringify("production"),
      }),
    ],
    // plugins,
  },
  {
    input,
    output: {
      file: pkg.module,
      format: "esm",
      sourcemap: true,
    },
    plugins,
  },
  {
    input,
    output: {
      file: pkg.main,
      format: "cjs",
      sourcemap: true,
    },
    plugins,
  },
];
