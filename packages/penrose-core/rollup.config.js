import typescript from "rollup-plugin-typescript2";
import pkg from "./package.json";

const input = "./src/index.ts";
const plugins = [
  typescript({
    tsconfig: "tsconfig.json",
  }),
];
export default [
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
