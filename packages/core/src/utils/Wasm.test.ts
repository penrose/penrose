import { describe, expect, test } from "vitest";
import { int } from "./Wasm.js";

describe("LEB128", () => {
  const leb128 = (n: number): number[] => {
    const bytes: number[] = [];
    const byte = (b: number) => {
      bytes.push(b);
    };
    int({ byte }, n);
    return bytes;
  };

  // https://en.wikipedia.org/wiki/LEB128#Encoding_format
  test("unsigned example", () => {
    expect(leb128(624485)).toEqual([0xe5, 0x8e, 0x26]);
  });
  test("signed example", () => {
    expect(leb128(-123456)).toEqual([0xc0, 0xbb, 0x78]);
  });

  test("zero", () => {
    expect(leb128(0)).toEqual([0x00]);
  });

  test("least positive", () => {
    expect(leb128(1)).toEqual([0x01]);
  });
  test("most positive 1-byte", () => {
    expect(leb128(63)).toEqual([0x3f]);
  });
  test("least positive 2-byte", () => {
    expect(leb128(64)).toEqual([0xc0, 0x00]);
  });
  test("most positive 2-byte", () => {
    expect(leb128(8191)).toEqual([0xff, 0x3f]);
  });
  test("least positive 3-byte", () => {
    expect(leb128(8192)).toEqual([0x80, 0xc0, 0x00]);
  });
  test("most positive 3-byte", () => {
    expect(leb128(1048575)).toEqual([0xff, 0xff, 0x3f]);
  });
  test("least positive 4-byte", () => {
    expect(leb128(1048576)).toEqual([0x80, 0x80, 0xc0, 0x00]);
  });
  test("most positive 4-byte", () => {
    expect(leb128(134217727)).toEqual([0xff, 0xff, 0xff, 0x3f]);
  });
  test("least positive 5-byte", () => {
    expect(leb128(134217728)).toEqual([0x80, 0x80, 0x80, 0xc0, 0x00]);
  });
  test("most positive", () => {
    expect(leb128(2147483647)).toEqual([0xff, 0xff, 0xff, 0xff, 0x07]);
  });
  test("least positive error", () => {
    expect(() => leb128(2147483648)).toThrow(
      "cannot represent 2147483648 as a 32-bit signed integer"
    );
  });

  test("least negative", () => {
    expect(leb128(-1)).toEqual([0x7f]);
  });
  test("most negative 1-byte", () => {
    expect(leb128(-64)).toEqual([0x40]);
  });
  test("least negative 2-byte", () => {
    expect(leb128(-65)).toEqual([0xbf, 0x7f]);
  });
  test("most negative 2-byte", () => {
    expect(leb128(-8192)).toEqual([0x80, 0x40]);
  });
  test("least negative 3-byte", () => {
    expect(leb128(-8193)).toEqual([0xff, 0xbf, 0x7f]);
  });
  test("most negative 3-byte", () => {
    expect(leb128(-1048576)).toEqual([0x80, 0x80, 0x40]);
  });
  test("least negative 4-byte", () => {
    expect(leb128(-1048577)).toEqual([0xff, 0xff, 0xbf, 0x7f]);
  });
  test("most negative 4-byte", () => {
    expect(leb128(-134217728)).toEqual([0x80, 0x80, 0x80, 0x40]);
  });
  test("least negative 5-byte", () => {
    expect(leb128(-134217729)).toEqual([0xff, 0xff, 0xff, 0xbf, 0x7f]);
  });
  test("most negative", () => {
    expect(leb128(-2147483648)).toEqual([0x80, 0x80, 0x80, 0x80, 0x78]);
  });
  test("least negative error", () => {
    expect(() => leb128(-2147483649)).toThrow(
      "cannot represent -2147483649 as a 32-bit signed integer"
    );
  });

  test("non-integer", () => {
    expect(() => leb128(Math.PI)).toThrow(
      "cannot represent 3.141592653589793 as a 32-bit signed integer"
    );
  });
});
