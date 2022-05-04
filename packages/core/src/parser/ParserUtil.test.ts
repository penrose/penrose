import * as moo from "moo";
import * as ParserUtil from "parser/ParserUtil";

describe("ParserUtil", () => {
  test("rangeOf", () => {
    const lexer = moo.compile({
      x: "x",
      y: { match: /[^x]+/, lineBreaks: true },
    });
    lexer.reset("asdf\n foo \r\n3\n21xyzw");
    expect(ParserUtil.rangeOf(lexer.next()!)).toEqual({
      start: { line: 1, col: 0 },
      end: { line: 4, col: 2 },
    });
    expect(ParserUtil.rangeOf(lexer.next()!)).toEqual({
      start: { line: 4, col: 2 },
      end: { line: 4, col: 3 },
    });
    expect(ParserUtil.rangeOf(lexer.next()!)).toEqual({
      start: { line: 4, col: 3 },
      end: { line: 4, col: 6 },
    });
    expect(lexer.next()).toBeUndefined();
  });
});
