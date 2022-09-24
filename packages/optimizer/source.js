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

  const withVec = (T, len, f) => {
    const align = T.BYTES_PER_ELEMENT;
    const size = len * align;
    const ptr = rust.__wbindgen_malloc(size, align);
    try {
      return f(new T(rust.memory.buffer, ptr, len));
    } finally {
      rust.__wbindgen_free(ptr, size, align);
    }
  };

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

  const index = rust.__indirect_function_table.length;
  rust.__indirect_function_table.grow(1);

  const samplerByte = rust.input_meta_sampler_byte();
  const pendingByte = rust.input_meta_pending_byte();
  const inputMetaToByte = (meta) => {
    switch (meta) {
      case "sampler":
        return samplerByte;
      case "pending":
        return pendingByte;
      default:
        throw new Error(`unknown input meta: ${meta}`);
    }
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

    converge: ({ inputs, numObjEngs, numConstrEngs, varyingValues }) => {
      const n = inputs.length;
      const nXs = varyingValues.length;
      if (nXs !== n) {
        throw new Error(
          `got ${nXs} varying values but ${n} input meta values; should match`
        );
      }

      return withVec(Uint8Array, n, (vInputs) => {
        for (let i = 0; i < n; i++) {
          vInputs[i] = inputMetaToByte(inputs[i]);
        }
        return withVec(Float64Array, n, (vXs) => {
          vXs.set(varyingValues);
          rust.converge(
            index,
            vInputs.byteOffset,
            n,
            numObjEngs,
            numConstrEngs,
            vXs.byteOffset,
            n
          );
          return Array.from(vXs);
        });
      });
    },
  };
};
