// https://webassembly.github.io/spec/core/binary/

//#region constants for the binary format

/** Section ids. */
export const SECTION = {
  TYPE: 1,
  IMPORT: 2,
  FUNCTION: 3,
  EXPORT: 7,
  CODE: 10,
};

/** `importdesc` leading bytes. */
export const IMPORT = { FUNCTION: 0x00, MEMORY: 0x02 };

/** `exportdesc` leading bytes. */
export const EXPORT = { FUNCTION: 0x00 };

/** Type bytes. */
export const TYPE = { FUNCTION: 0x60, f64: 0x7c, i32: 0x7f };

/** `limits` leading bytes. */
export const LIMITS = { NO_MAXIMUM: 0x00 };

/** Instruction opcodes. */
export const OP = {
  if: 0x04,
  call: 0x10,
  drop: 0x1a,
  select: 0x1b,

  f64: {
    load: 0x2b,
    store: 0x39,
    const: 0x44,
    eq: 0x61,
    lt: 0x63,
    gt: 0x64,
    le: 0x65,
    ge: 0x66,
    abs: 0x99,
    neg: 0x9a,
    ceil: 0x9b,
    floor: 0x9c,
    trunc: 0x9d,
    nearest: 0x9e,
    sqrt: 0x9f,
    add: 0xa0,
    sub: 0xa1,
    mul: 0xa2,
    div: 0xa3,
    min: 0xa4,
    max: 0xa5,
  },
  i32: {
    load: 0x28,
    const: 0x41,
    eqz: 0x45,
    add: 0x6a,
    and: 0x71,
    or: 0x72,
    xor: 0x73,
  },
  local: { get: 0x20, set: 0x21 },
};

/** `end` opcode. */
export const END = 0x0b;

//#endregion

//#region helper types, functions, and classes

/**
 * A mutable object to which individual bytes can be written. Exported only for
 * testing purposes; prefer `Target`.
 */
export interface Byter {
  /**
   * Write a single byte.
   * @param b the byte
   */
  byte(b: number): void;
}

/**
 * Writes a 32-bit signed integer to a target. Exported only for testing
 * purposes; prefer `Count`, `intSize`, or `Module` depending on your use case.
 * @param t target
 * @param n integer to write
 */
export const int = (t: Byter, n: number): void => {
  if (!(Number.isInteger(n) && -0x80000001 < n && n < 0x80000000))
    throw new Error(`cannot represent ${n} as a 32-bit signed integer`);
  while (n < -0x40 || 0x3f < n) {
    t.byte((n & 0x7f) | 0x80);
    n >>= 7;
  }
  t.byte(n & 0x7f);
};

/**
 * A mutable object to which bytes, 32-bit signed integers, 64-bit
 * floating-point numbers, and ASCII strings can be written.
 */
export interface Target extends Byter {
  /**
   * Write a LEB128-encoded 32-bit signed integer.
   * @param n the integer
   */
  int(n: number): void;
  /**
   * Write an IEEE 754-2019 little-endian 64-bit floating-point number.
   * @param x the number
   */
  f64(x: number): void;
  /**
   * Write an ASCII string as a vector of bytes.
   * @param s the string
   */
  ascii(s: string): void;
}

/**
 * A `Target` that doesn't actually put bytes anywhere, and instead just counts
 * how many bytes would be written. Useful for calculating the size of a
 * `Module` before allocating one big buffer to store its bytes, because
 * JavaScript typed arrays cannot be resized after construction.
 */
export class Count implements Target {
  size = 0;

  byte(): void {
    this.size++;
  }

  int(n: number): void {
    int(this, n);
  }

  f64(): void {
    this.size += Float64Array.BYTES_PER_ELEMENT;
  }

  ascii(s: string): void {
    this.int(s.length);
    this.size += s.length;
  }
}

/**
 * @param n integer to encode
 * @returns number of bytes to encode as signed LEB128
 */
export const intSize = (n: number): number => {
  const count = new Count();
  count.int(n);
  return count.size;
};

const PREAMBLE_SIZE = 8;

/**
 * A `Target` which preallocates a byte buffer of known size (perhaps computed
 * via `Count`) with the standard 8-byte preamble, and writes data to it,
 * incrementing an internal byte index as it goes.
 */
export class Module implements Target {
  bytes: Uint8Array;
  count: Count;
  floatIn: Float64Array;
  floatOut: Uint8Array;

  /**
   * Allocates a byte buffer for a Wasm module, automatically adding the
   * standard 8 bytes for the preamble.
   * @param sumSectionSizes number of bytes in the module, minus 8 for preamble
   */
  constructor(sumSectionSizes: number) {
    this.bytes = new Uint8Array(PREAMBLE_SIZE + sumSectionSizes);
    this.count = new Count();

    const buf = new ArrayBuffer(Float64Array.BYTES_PER_ELEMENT);
    this.floatIn = new Float64Array(buf);
    this.floatOut = new Uint8Array(buf);

    // magic
    this.byte(0x00);
    this.byte(0x61);
    this.byte(0x73);
    this.byte(0x6d);

    // version
    this.byte(0x01);
    this.byte(0x00);
    this.byte(0x00);
    this.byte(0x00);
  }

  byte(b: number): void {
    this.bytes[this.count.size] = b;
    this.count.byte();
  }

  int(n: number): void {
    int(this, n);
  }

  f64(x: number): void {
    // https://stackoverflow.com/a/7870190 this assumes that the underlying
    // hardware is little-endian, and will break if that assumption is violated;
    // it may be wiser to check the endianness and then flip the order of these
    // bytes, but the downside is there isn't an easy way to test that
    this.floatIn[0] = x;
    this.bytes.set(this.floatOut, this.count.size);
    this.count.f64();
  }

  ascii(s: string): void {
    this.int(s.length);
    for (let i = 0; i < s.length; i++) {
      const c = s.charCodeAt(i);
      if (c > 127)
        throw Error(`non-ASCII at index ${i} in ${JSON.stringify(s)}`);
      this.byte(c);
    }
  }
}

//#endregion
