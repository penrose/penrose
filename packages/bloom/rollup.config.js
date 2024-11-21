import commonjs from "@rollup/plugin-commonjs";
import { nodeResolve } from "@rollup/plugin-node-resolve";
import replace from "@rollup/plugin-replace";
import { minify } from "rollup-plugin-esbuild-minify";

export default {
  plugins: [
    commonjs(),
    nodeResolve(),
    minify(),
    replace({
      "process.env.NODE_ENV": JSON.stringify("production"),
      preventAssignment: true,
    }),
  ],
  input: "dist/index_no_react.js",
  output: {
    file: "dist/bloom.min.js",
    format: "es",
  },
};
