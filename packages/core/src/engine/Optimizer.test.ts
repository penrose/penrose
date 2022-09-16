import { getOptimizer } from "@penrose/optimizer";

const makeModule = () => {
  const arr = new Uint8Array(53);

  // https://webassembly.github.io/spec/core/binary/modules.html#binary-module

  // magic
  arr[0] = 0x00;
  arr[1] = 0x61;
  arr[2] = 0x73;
  arr[3] = 0x6d;

  // version
  arr[4] = 0x01;
  arr[5] = 0x00;
  arr[6] = 0x00;
  arr[7] = 0x00;

  // https://webassembly.github.io/spec/core/binary/modules.html#binary-typesec
  arr[8] = 0x01; // type section

  // https://webassembly.github.io/spec/core/binary/modules.html#sections
  // https://webassembly.github.io/spec/core/binary/values.html#integers
  // unsigned LEB128 encoded number of bytes in the type section contents
  arr[9] = 0x06;

  // https://webassembly.github.io/spec/core/binary/conventions.html#binary-vec
  arr[10] = 0x01; // number of functions in the type section

  // https://webassembly.github.io/spec/core/binary/types.html#binary-functype
  arr[11] = 0x60; // function type

  arr[12] = 0x01; // number of parameters

  // https://webassembly.github.io/spec/core/binary/types.html#number-types
  arr[13] = 0x7f; // parameter is i32

  arr[14] = 0x01; // only one return type
  arr[15] = 0x7c; // return type is f64

  // https://webassembly.github.io/spec/core/binary/modules.html#import-section
  arr[16] = 0x02; // import section
  arr[17] = 0x0b; // number of bytes in the import section
  arr[18] = 0x01; // number of imports in the import section

  // https://webassembly.github.io/spec/core/binary/values.html#binary-name
  arr[19] = 0x02; // number of bytes in the module name

  // module name
  arr[20] = 0x6a; // j
  arr[21] = 0x73; // s

  arr[22] = 0x03; // number of bytes in the import name

  // import name
  arr[23] = 0x6d; // m
  arr[24] = 0x65; // e
  arr[25] = 0x6d; // m

  arr[26] = 0x02; // memory import

  // https://webassembly.github.io/spec/core/binary/types.html#binary-memtype
  arr[27] = 0x00; // no maximum
  arr[28] = 0x01; // minimum of 1 page

  // https://webassembly.github.io/spec/core/binary/modules.html#binary-funcsec
  arr[29] = 0x03; // function section
  arr[30] = 0x02; // number of bytes in the function section contents
  arr[31] = 0x01; // number of type indices in the function section
  arr[32] = 0x00; // type index of the first function

  // https://webassembly.github.io/spec/core/binary/modules.html#export-section
  arr[33] = 0x07; // export section
  arr[34] = 0x07; // number of bytes in the export section contents
  arr[35] = 0x01; // number of exports in the export section

  arr[36] = 0x03; // number of bytes in the export name

  // export name
  arr[37] = 0x67; // g
  arr[38] = 0x65; // e
  arr[39] = 0x74; // t

  arr[40] = 0x00; // function index
  arr[41] = 0x00; // first function

  // https://webassembly.github.io/spec/core/binary/modules.html#code-section
  arr[42] = 0x0a; // code section
  arr[43] = 0x09; // number of bytes in the code section contents
  arr[44] = 0x01; // number of function bodies in the code section

  arr[45] = 0x07; // number of bytes in the first function body

  arr[46] = 0x00; // number of local variables

  // https://webassembly.github.io/spec/core/binary/instructions.html#variable-instructions
  arr[47] = 0x20; // local.get
  arr[48] = 0x00; // 0

  // https://webassembly.github.io/spec/core/binary/instructions.html#memory-instructions
  arr[49] = 0x2b; // f64.load
  arr[50] = 0x00; // align 0
  arr[51] = 0x00; // offset 0

  // https://webassembly.github.io/spec/core/binary/instructions.html#binary-expr
  arr[52] = 0x0b; // end of expression

  return arr;
};

describe("optimizer", () => {
  test("works", async () => {
    const opt = await getOptimizer();
    const f = (
      await WebAssembly.instantiate(makeModule(), { js: { mem: opt.memory } })
    ).instance.exports.get;
    opt.__indirect_function_table.set(0, f);
    expect(opt.step(0, -42)).toBe(42);
  });
});
