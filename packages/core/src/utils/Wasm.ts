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
  f64: {
    abs: 0x99,
    add: 0xa0,
    ceil: 0x9b,
    const: 0x44,
    div: 0xa3,
    eq: 0x61,
    floor: 0x9c,
    ge: 0x66,
    gt: 0x64,
    load: 0x2b,
    le: 0x65,
    lt: 0x63,
    max: 0xa5,
    min: 0xa4,
    mul: 0xa2,
    nearest: 0x9e,
    neg: 0x9a,
    sqrt: 0x9f,
    store: 0x39,
    sub: 0xa1,
    trunc: 0x9d,
  },
  i32: { add: 0x6a, and: 0x71, const: 0x41, eqz: 0x45, or: 0x72 },
  local: { get: 0x20, set: 0x21 },
  select: 0x1b,
};

export const END = 0x0b;

export interface Target {
  byte(b: number): void;
  int(n: number): void;
  f64(x: number): void;
  ascii(s: string): void;
}

const int = (t: Target, n: number): void => {
  // https://en.wikipedia.org/wiki/LEB128#Encode_unsigned_integer
  do {
    let byte = n & 0x7f;
    n >>= 7;
    if (n > 0) byte |= 0x80;
    t.byte(byte);
  } while (n > 0);
};

export class Count implements Target {
  size: number;

  constructor() {
    this.size = 0;
  }

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

export const intSize = (n: number): number => {
  const count = new Count();
  count.int(n);
  return count.size;
};

export class Module implements Target {
  bytes: Uint8Array;
  count: Count;
  floatIn: Float64Array;
  floatOut: Uint8Array;

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
