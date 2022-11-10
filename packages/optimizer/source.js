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

const mangle = (name) => `penrose_${name}`;

export const getOptimizer = async (gradient, shapes) => {
  const rust = (await WebAssembly.instantiate(src)).instance.exports;

  const index = rust.__indirect_function_table.length;
  rust.__indirect_function_table.grow(1);

  const jitGrad = (
    await WebAssembly.instantiate(gradient, {
      [importMemoryModule]: { [importMemoryName]: rust.memory },
    })
  ).instance.exports;
  builtins.forEach((name, i) => {
    jitGrad[exportTableName].set(i, rust[mangle(name)]);
  });
  rust.__indirect_function_table.set(index, jitGrad[exportFunctionName]);

  const jitShapes = (
    await WebAssembly.instantiate(shapes, {
      [importMemoryModule]: { [importMemoryName]: rust.memory },
    })
  ).instance.exports;
  builtins.forEach((name, i) => {
    jitShapes[exportTableName].set(i, rust[mangle(name)]);
  });

  const withVec = (T, len, f) => {
    const align = T.BYTES_PER_ELEMENT;
    const size = len * align;
    const ptr = rust.__wbindgen_malloc(size, align);
    try {
      return f(() => new T(rust.memory.buffer, ptr, len));
    } finally {
      rust.__wbindgen_free(ptr, size, align);
    }
  };

  const samplerByte = rust.penrose_input_meta_sampler_byte();
  const pendingByte = rust.penrose_input_meta_pending_byte();
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
    converge: ({ inputs, numObjEngs, numConstrEngs, varyingValues }) => {
      const n = inputs.length;
      const nXs = varyingValues.length;
      if (nXs !== n) {
        throw new Error(
          `got ${nXs} varying values but ${n} input meta values; should match`
        );
      }

      return withVec(Uint8Array, n, (viewInputs) => {
        const vInputs = viewInputs();
        for (let i = 0; i < n; i++) {
          vInputs[i] = inputMetaToByte(inputs[i]);
        }
        return withVec(Float64Array, n, (viewXs) => {
          const vXs = viewXs();
          vXs.set(varyingValues);
          rust.penrose_converge(
            index,
            vInputs.byteOffset,
            n,
            numObjEngs,
            numConstrEngs,
            vXs.byteOffset,
            n
          );
          return Array.from(viewXs());
        });
      });
    },

    shapes: (inputs, lenSecondary) =>
      withVec(Float64Array, inputs.length, (viewInputs) =>
        withVec(Float64Array, inputs.length, (viewGradient) =>
          withVec(Float64Array, lenSecondary, (viewSecondary) => {
            const vInputs = viewInputs();
            vInputs.set(inputs);
            jitShapes[exportFunctionName](
              vInputs.byteOffset,
              viewGradient().byteOffset,
              viewSecondary().byteOffset
            );
            return Array.from(viewSecondary());
          })
        )
      ),
  };
};
