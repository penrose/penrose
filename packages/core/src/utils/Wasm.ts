// https://webassembly.github.io/spec/core/binary/

export const PREAMBLE_SIZE = 8;

export const SECTION = {
  TYPE: 1,
  IMPORT: 2,
  FUNCTION: 3,
  TABLE: 4,
  EXPORT: 7,
  CODE: 10,
};

export const IMPORT = { MEMORY: 0x02 };

export const EXPORT = { FUNCTION: 0x00, TABLE: 0x01 };

export const TYPE = { i32: 0x7f, f64: 0x7c, FUNCREF: 0x70, FUNCTION: 0x60 };

export const LIMITS = { NO_MAXIMUM: 0x00, MAXIMUM: 0x01 };

export const OP = {
  call_indirect: 0x11,
  f64: { load: 0x2b },
  i32: { const: 0x41 },
  local: { get: 0x20 },
};

export const END = 0x0b;

export const expectSize = (
  name: string,
  expected: number,
  actual: number
): void => {
  if (actual !== expected)
    throw new Error(`${name} size is ${actual}, expected ${expected}`);
};

const expectSmall = (n: number): void => {
  if (n > 127) throw Error(`cannot LEB128-encode ${n}, too big`);
};

export const unsignedLEB128Size = (n: number): number => {
  expectSmall(n);
  return 1;
};

export const unsignedLEB128 = (
  bytes: Uint8Array,
  offset: number,
  n: number
): number => {
  expectSmall(n);
  bytes[offset++] = n;
  return offset;
};

export const ascii = (bytes: Uint8Array, offset: number, s: string): number => {
  offset = unsignedLEB128(bytes, offset, s.length);
  for (let i = 0; i < s.length; i++) {
    const c = s.charCodeAt(i);
    if (c > 127) throw Error(`non-ASCII at index ${i} in ${JSON.stringify(s)}`);
    bytes[offset++] = c;
  }
  return offset;
};

export const module = (sumSectionSizes: number): Uint8Array => {
  const bytes = new Uint8Array(PREAMBLE_SIZE + sumSectionSizes);

  // magic
  bytes[0] = 0x00;
  bytes[1] = 0x61;
  bytes[2] = 0x73;
  bytes[3] = 0x6d;

  // version
  bytes[4] = 0x01;
  bytes[5] = 0x00;
  bytes[6] = 0x00;
  bytes[7] = 0x00;

  return bytes;
};
