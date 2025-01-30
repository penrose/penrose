import { describe, expect, test } from "vitest";
import { parser } from "./SubstanceParser.js";

const compare = (progText: string) => {
  const tree = parser.parse(progText);
  expect(tree.toString()).not.toContain("⚠"); // No error nodes
};

describe("Common", () => {
  test("empty program", () => {
    compare("");
  });

  test("comments and white spaces", () => {
    const prog = `
-- Top-level comments
Set A, B, C, D, E, F, G -- inline comments\r\n

/*
Subset(B, A)\r
Subset(C, A)\r\n
Subset(D, B)
Subset(E, B)
Subset(F, C)
Subset(G, C)
*/

-- Not(Intersecting(E, D))
Set C
-- Not(Intersecting(B, C))
AutoLabel All

/* Other comments */
    `;
    compare(prog);
  });

  test("no trailing newline", () => {
    const prog = `
Set A
Set B
Set C
Set D
Set E`;
    compare(prog);
  });

  test("trailing comment", () => {
    const prog = `
Set A
Set B
Set C
Set D
-- Set E`;
    compare(prog);
  });
});

describe("statements", () => {
  describe("isets", () => {
    test.each([
      "Set A for i in [0, 10]",
      "Set A for i in [0, 10], j in [1, 5]",
    ])("decl iset %s", (iset: string) => {
      compare(iset);
    });

    test.each([
      "Set A := MakeSet(hello_j) for j in [0, 20]",
      "Let B := Set(hello_world) for abc in [80, 70]",
    ])("declbind iset %s", (iset: string) => {
      compare(iset);
    });

    test.each([
      "Edge(a_i, a_j) for i in [0, 20], j in [20, 30] where i + 1 == j && j + 1 == i || !(j == 1 && y == 2)",
      "Edge(v_i, v_i) for i in [0, 20] where 20 != 20",
    ])("pred conditional iset %s", (iset: string) => {
      compare(iset);
    });

    test.each([
      'Label x_i "abcde" for i in [0, 10]',
      "Label y $abc$ for j in [0, 15]",
    ])("label iset %s", (iset: string) => {
      compare(iset);
    });
  });

  test("decl and decl list", () => {
    const prog = `
Set A
Map f, g, h
    `;
    compare(prog);
  });

  test.each(["Set a", "Set a, b"])("decl list %s", (iset: string) => {
    compare(iset);
  });

  test("label decl", () => {
    const prog = `
Set A, B, C
Label A $\\vec{A}$
Label B $B_1$
    `;
    compare(prog);
  });

  test("no label decl", () => {
    const prog = `
Set A, B, C
NoLabel A
NoLabel B, C
    `;
    compare(prog);
  });

  test("auto label decl", () => {
    const prog = `
Set A, B, C
AutoLabel All
AutoLabel B, C
NoLabel B, C
    `;
    compare(prog);
  });

  test("bind and exprs", () => {
    const prog = `
Set A, B, C
Point p1, p2
C := Intersection(A, B)
    `;
    compare(prog);
  });

  test("predicates", () => {
    const prog = `
Set A, B, C
Subset(A, B)
    `;
    compare(prog);
  });

  test("numbers and strings", () => {
    const prog = `
    Set A
    Contains(A, 1)
    Contains(B, "never gonna give you up")
        `;
    compare(prog);
  });
});
