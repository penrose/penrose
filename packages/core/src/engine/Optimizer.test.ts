import {
  builtins,
  exportFunctionName,
  exportTableName,
  getOptimizer,
  importMemoryModule,
  importMemoryName,
} from "@penrose/optimizer";
import * as wasm from "utils/Wasm";

const makeModule = (): Uint8Array => {
  const numSections = 6;

  const typeSectionSize = 11;
  const importSectionSize = 21;
  const functionSectionSize = 2;
  const tableSectionSize = 5;
  const exportSectionSize = 16;
  const codeSectionSize = 14;

  const sumSectionSizes =
    numSections +
    wasm.unsignedLEB128Size(typeSectionSize) +
    typeSectionSize +
    wasm.unsignedLEB128Size(importSectionSize) +
    importSectionSize +
    wasm.unsignedLEB128Size(functionSectionSize) +
    functionSectionSize +
    wasm.unsignedLEB128Size(tableSectionSize) +
    tableSectionSize +
    wasm.unsignedLEB128Size(exportSectionSize) +
    exportSectionSize +
    wasm.unsignedLEB128Size(codeSectionSize) +
    codeSectionSize;

  const bytes = wasm.module(sumSectionSizes);

  const numFunctions = 1;

  let offset = wasm.PREAMBLE_SIZE;

  bytes[offset++] = wasm.SECTION.TYPE;
  offset = wasm.unsignedLEB128(bytes, offset, typeSectionSize);
  {
    const start = offset;

    const numFuncTypes = 2;
    bytes[offset++] = numFuncTypes;
    {
      const numParams = 1;
      const numReturns = 1;
      bytes[offset++] = wasm.TYPE.FUNCTION;
      offset = wasm.unsignedLEB128(bytes, offset, numParams);
      bytes[offset++] = wasm.TYPE.i32;
      offset = wasm.unsignedLEB128(bytes, offset, numReturns);
      bytes[offset++] = wasm.TYPE.f64;
    }
    {
      const numParams = 1;
      const numReturns = 1;
      bytes[offset++] = wasm.TYPE.FUNCTION;
      offset = wasm.unsignedLEB128(bytes, offset, numParams);
      bytes[offset++] = wasm.TYPE.f64;
      offset = wasm.unsignedLEB128(bytes, offset, numReturns);
      bytes[offset++] = wasm.TYPE.f64;
    }

    wasm.expectSize("type section", typeSectionSize, offset - start);
  }

  bytes[offset++] = wasm.SECTION.IMPORT;
  offset = wasm.unsignedLEB128(bytes, offset, importSectionSize);
  {
    const start = offset;

    const numImports = 1;
    offset = wasm.unsignedLEB128(bytes, offset, numImports);
    {
      const minPages = 1;
      offset = wasm.ascii(bytes, offset, importMemoryModule);
      offset = wasm.ascii(bytes, offset, importMemoryName);
      bytes[offset++] = wasm.IMPORT.MEMORY;
      bytes[offset++] = wasm.LIMITS.NO_MAXIMUM;
      offset = wasm.unsignedLEB128(bytes, offset, minPages);
    }

    wasm.expectSize("import section", importSectionSize, offset - start);
  }

  bytes[offset++] = wasm.SECTION.FUNCTION;
  offset = wasm.unsignedLEB128(bytes, offset, functionSectionSize);
  {
    const start = offset;

    offset = wasm.unsignedLEB128(bytes, offset, numFunctions);
    {
      const typeIndex = 0;
      offset = wasm.unsignedLEB128(bytes, offset, typeIndex);
    }

    wasm.expectSize("function section", functionSectionSize, offset - start);
  }

  bytes[offset++] = wasm.SECTION.TABLE;
  offset = wasm.unsignedLEB128(bytes, offset, tableSectionSize);
  {
    const start = offset;

    const numTables = 1;
    offset = wasm.unsignedLEB128(bytes, offset, numTables);
    {
      const minEntries = builtins.length;
      const maxEntries = builtins.length;
      bytes[offset++] = wasm.TYPE.FUNCREF;
      bytes[offset++] = wasm.LIMITS.MAXIMUM;
      offset = wasm.unsignedLEB128(bytes, offset, minEntries);
      offset = wasm.unsignedLEB128(bytes, offset, maxEntries);
    }

    wasm.expectSize("table section", tableSectionSize, offset - start);
  }

  bytes[offset++] = wasm.SECTION.EXPORT;
  offset = wasm.unsignedLEB128(bytes, offset, exportSectionSize);
  {
    const start = offset;

    const numExports = 2;
    offset = wasm.unsignedLEB128(bytes, offset, numExports);
    {
      const tableIndex = 0;
      offset = wasm.ascii(bytes, offset, exportTableName);
      bytes[offset++] = wasm.EXPORT.TABLE;
      offset = wasm.unsignedLEB128(bytes, offset, tableIndex);
    }
    {
      const funcIndex = 0;
      offset = wasm.ascii(bytes, offset, exportFunctionName);
      bytes[offset++] = wasm.EXPORT.FUNCTION;
      offset = wasm.unsignedLEB128(bytes, offset, funcIndex);
    }

    wasm.expectSize("export section", exportSectionSize, offset - start);
  }

  bytes[offset++] = wasm.SECTION.CODE;
  offset = wasm.unsignedLEB128(bytes, offset, codeSectionSize);
  {
    const start = offset;

    bytes[offset++] = numFunctions;
    {
      const numBytes = 12;
      const numLocals = 0;
      offset = wasm.unsignedLEB128(bytes, offset, numBytes);
      offset = wasm.unsignedLEB128(bytes, offset, numLocals);
      {
        bytes[offset++] = wasm.OP.local.get;
        offset = wasm.unsignedLEB128(bytes, offset, 0);
      }
      {
        bytes[offset++] = wasm.OP.f64.load;
        offset = wasm.unsignedLEB128(bytes, offset, 0);
        offset = wasm.unsignedLEB128(bytes, offset, 0);
      }
      {
        bytes[offset++] = wasm.OP.i32.const;
        offset = wasm.unsignedLEB128(bytes, offset, builtins.indexOf("log"));
      }
      {
        bytes[offset++] = wasm.OP.call_indirect;
        offset = wasm.unsignedLEB128(bytes, offset, 1);
        offset = wasm.unsignedLEB128(bytes, offset, 0);
      }
      bytes[offset++] = wasm.END;
    }

    wasm.expectSize("code section", codeSectionSize, offset - start);
  }

  wasm.expectSize("module", bytes.length, offset);

  return bytes;
};

describe("optimizer", () => {
  test("works", async () => {
    const optimizer = await getOptimizer();
    await optimizer.link(makeModule());
    expect(optimizer.step(-42)).toBeCloseTo(3.7376696182833684);
  });
});
