import src from "./target/wasm32-unknown-unknown/release/penrose_optimizer.wasm";

export const importMemoryModule = "optimizer";
export const importMemoryName = "memory";

export const exportTableName = "builtins";
export const exportFunctionName = "f";

export const builtins = [
  "acos",
  "acosh",
  "asin",
  "asinh",
  "atan",
  "atanh",
  "atan2",
  "cbrt",
  "cos",
  "cosh",
  "exp",
  "expm1",
  "log",
  "log1p",
  "log10",
  "log2",
  "pow",
  "sin",
  "sinh",
  "tan",
  "tanh",
];

export const getOptimizer = async () => {
  const rust = (await WebAssembly.instantiate(src)).instance.exports;
  const index = rust.__indirect_function_table.length;
  rust.__indirect_function_table.grow(1);
  const definitions = {
    acos: rust.arccosine,
    acosh: rust.hyperbolic_arccosine,
    asin: rust.arcsine,
    asinh: rust.hyperbolic_arcsine,
    atan: rust.arctangent,
    atanh: rust.hyperbolic_arctangent,
    atan2: rust.four_quadrant_arctangent,
    cbrt: rust.cube_root,
    cos: rust.cosine,
    cosh: rust.hyperbolic_cosine,
    exp: rust.exponential,
    expm1: rust.exponential_minus_one,
    log: rust.natural_logarithm,
    log1p: rust.natural_logarithm_one_plus,
    log10: rust.base_ten_logarithm,
    log2: rust.base_two_logarithm,
    pow: rust.power,
    sin: rust.sine,
    sinh: rust.hyperbolic_sine,
    tan: rust.tangent,
    tanh: rust.hyperbolic_tangent,
  };
  return {
    link: async (source) => {
      const jit = (
        await WebAssembly.instantiate(source, {
          [importMemoryModule]: { [importMemoryName]: rust.memory },
        })
      ).instance.exports;
      builtins.forEach((name, i) => {
        jit[exportTableName].set(i, definitions[name]);
      });
      rust.__indirect_function_table.set(index, jit[exportFunctionName]);
    },
    step: (x) => rust.step(index, x),
  };
};
