// https://webassembly.github.io/spec/core/binary/

export const PREAMBLE_SIZE = 8;

export const SECTION = {
  TYPE: 1,
  IMPORT: 2,
  FUNCTION: 3,
  EXPORT: 7,
  CODE: 10,
};

export const IMPORT = { FUNCTION: 0x00, MEMORY: 0x02 };

export const EXPORT = { FUNCTION: 0x00 };

export const TYPE = { FUNCTION: 0x60, f64: 0x7c, i32: 0x7f };

export const LIMITS = { NO_MAXIMUM: 0x00 };

export const OP = {
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
  i32: { const: 0x41, eqz: 0x45, add: 0x6a, and: 0x71, or: 0x72 },
  local: { get: 0x20, set: 0x21, tee: 0x22 },
};

export const END = 0x0b;

export interface Byter {
  byte(b: number): void;
}

export const int = (t: Byter, n: number): void => {
  if (!(Number.isInteger(n) && -0x80000001 < n && n < 0x80000000))
    throw new Error(`cannot represent ${n} as a 32-bit signed integer`);
  while (n < -0x40 || 0x3f < n) {
    t.byte((n & 0x7f) | 0x80);
    n >>= 7;
  }
  t.byte(n & 0x7f);
};

export interface Target extends Byter {
  int(n: number): void;
  f64(x: number): void;
  ascii(s: string): void;
}

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
