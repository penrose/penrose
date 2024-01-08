import nearley from "nearley";
import { beforeEach, describe, expect, test } from "vitest";
import grammar from "./SubstanceParser.js";

let parser: nearley.Parser;
const sameASTs = (results: any[]) => {
  for (const p of results) expect(results[0]).toEqual(p);
  expect(results.length).toEqual(1);
};

beforeEach(() => {
  // NOTE: Neither `feed` nor `finish` will reset the parser state. Therefore recompiling before each unit test
  parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));
});

describe("Common", () => {
  test("empty program", () => {
    const { results } = parser.feed("");
    sameASTs(results);
  });
  test("comments and white spaces", () => {
    const prog = `
-- Top-level comments
Set A, B, C, D, E, F, G -- inline comments\r\n

/*
IsSubset(B, A)\r
IsSubset(C, A)\r\n
IsSubset(D, B)
IsSubset(E, B)
IsSubset(F, C)
IsSubset(G, C)
*/

-- Not(Intersecting(E, D))
Set C
-- Not(Intersecting(B, C))
AutoLabel All

/* Other comments */

    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
  test("no trailing newline", () => {
    const prog = `
Set A
Set B
Set C
Set D
Set E`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
  test("trailing comment", () => {
    const prog = `
Set A
Set B
Set C
Set D
-- Set E`;
    const { results } = parser.feed(prog).feed("\n");
    sameASTs(results);
  });
});

describe("statements", () => {
  describe("isets", () => {
    test.each([
      "Set A for i in [0, 10]",
      "Set A for i in [0, 10], j in [1, 5]",
    ])("decl iset %s", (iset: string) => {
      const { results } = parser.feed(iset);
      sameASTs(results);
    });

    test.each([
      "Set A := MakeSet(hello_j) for j in [0, 20]",
      "Let B := Set(hello_world) for abc in [80, 70]",
    ])("declbind iset %s", (iset: string) => {
      const { results } = parser.feed(iset);
      sameASTs(results);
    });

    test.each([
      "Edge(a_i, a_j) for i in [0, 20], j in [20, 30] where i + 1 == j && j + 1 == i || !(j == 1 && y == 2)",
      "Edge(v_i, v_i) for i in [0, 20] where 20 != 20",
    ])("pred conditional iset %s", (iset: string) => {
      const { results } = parser.feed(iset);
      sameASTs(results);
    });

    test.each([
      'Label x_i "abcde" for i in [0, 10]',
      "Label y $abc$ for j in [0, 15]",
    ])("label iset %s", (iset: string) => {
      const { results } = parser.feed(iset);
      sameASTs(results);
    });
  });

  test("decl and decl list", () => {
    const prog = `
Set A
Map f, g, h
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
    expect(
      results[0].statements
        .map((statement: any) => {
          if (statement.tag === "Decl") {
            return statement.name.value;
          } else if (statement.tag === "DeclList") {
            return statement.names.map((n: any) => n.value);
          }
        })
        .flat(),
    ).toEqual(["A", "f", "g", "h"]);
  });

  test.each(["Set a", "Set a, b"])("decl list %s", (iset: string) => {
    const { results } = parser.feed(iset);

    sameASTs(results);
  });

  test("label decl", () => {
    const prog = `
Set A, B, C
Label A $\\vec{A}$
Label B $B_1$
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
    expect(results[0].statements[1].label.contents).toEqual("\\vec{A}");
    expect(results[0].statements[2].label.contents).toEqual("B_1");
  });
  test("no label decl", () => {
    const prog = `
Set A, B, C
NoLabel A
NoLabel B, C
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
    expect(results[0].statements[1].args[0].value).toEqual("A");
    expect(results[0].statements[2].args.map((a: any) => a.value)).toEqual([
      "B",
      "C",
    ]);
  });
  test("auto label decl", () => {
    const prog = `
Set A, B, C
AutoLabel All
AutoLabel B, C
NoLabel B, C
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
    expect(results[0].statements[1].option.tag).toEqual("DefaultLabels");
    expect(
      results[0].statements[2].option.variables.map((a: any) => a.value),
    ).toEqual(["B", "C"]);
  });
  test("bind and exprs", () => {
    const prog = `
Set A, B, C
Point p1, p2
C := Intersection(A, B)
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
  test("predicates", () => {
    const prog = `
Set A, B, C
IsSubset(A, B)
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
});
