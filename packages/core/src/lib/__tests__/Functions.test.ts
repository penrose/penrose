import { describe, expect, test } from "vitest";
import { simpleContext } from "../../index.js";
import * as ad from "../../types/ad.js";
import { Str } from "../../types/value.js";
import { evalStr } from "../../utils/Util.js";
import { compDict } from "../Functions.js";

describe("key-name equality", () => {
  test("each function's key and name should be equal", () => {
    for (const [name, func] of Object.entries(compDict)) {
      expect(name).toEqual(func.name);
    }
  });
});

test("TeXify", () => {
  const s: Str<ad.Num> = {
    tag: "ConcatStr",
    left: {
      tag: "ConstStr",
      contents: "hello_hh",
    },
    right: {
      tag: "ConcatStr",
      left: {
        tag: "ConcatStr",
        left: {
          tag: "FromNum",
          num: 0.2345678,
        },
        right: {
          tag: "ConstStr",
          contents: "_",
        },
      },
      right: {
        tag: "FromNum",
        num: -123.45,
      },
    },
  };

  const res = compDict["TeXify"].body(simpleContext("test"), s).value.contents;

  const resStr = evalStr(res);

  expect(resStr).toEqual("{hello}_{{hh0.2345678}_{{-123.45}}}");
});
