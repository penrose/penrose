import setTheory from "@penrose/examples/dist/set-theory-domain";
import * as fs from "fs";
import nearley from "nearley";
import * as path from "path";
import { isKeyOf } from "../utils/Util";
import grammar from "./SubstanceParser";

const outputDir = "/tmp/asts";
const saveASTs = false;

let parser: nearley.Parser;
const sameASTs = (results: any[]) => {
  for (const p of results) expect(results[0]).toEqual(p);
  expect(results.length).toEqual(1);
};

// USAGE:
// printAST(results[0])
const printAST = (ast: any) => {
  console.log(JSON.stringify(ast));
};

const subPaths = [
  "tree.substance",
  "continuousmap.substance",
  "twosets-simple.substance",
  "multisets.substance",
  "nested.substance",
];

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
  test("decl and decl list", () => {
    const prog = `
Set A
Map f, g, h
List(Set) l
List(Map) l1
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
    expect(results[0].statements.map((s: any) => s.name.value)).toEqual([
      "A",
      "f",
      "g",
      "h",
      "l",
      "l1",
    ]);
  });
  test("label decl", () => {
    const prog = `
Set A, B, C
Label A $\\vec{A}$
Label B $B_1$
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
    expect(results[0].statements[3].label.contents).toEqual("\\vec{A}");
    expect(results[0].statements[4].label.contents).toEqual("B_1");
  });
  test("no label decl", () => {
    const prog = `
Set A, B, C
NoLabel A
NoLabel B, C
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
    expect(results[0].statements[3].args[0].value).toEqual("A");
    expect(results[0].statements[4].args.map((a: any) => a.value)).toEqual([
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
    expect(results[0].statements[3].option.tag).toEqual("DefaultLabels");
    expect(
      results[0].statements[4].option.variables.map((a: any) => a.value)
    ).toEqual(["B", "C"]);
  });
  test("bind and exprs", () => {
    const prog = `
Set A, B, C
Point p1, p2
C := Intersection(A, B)
p1 := ValueOf(A.value)
p2 := ValueOf(B.value)
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
  test("predicates", () => {
    const prog = `
Set A, B, C
IsSubset(A, B)
Not(IsSubset(A, B))
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
  test("predicates", () => {
    const prog = `
Set A, B, C
IsSubset(Not(A), B) <-> IsSubset(B, C)
CreateSubset(A, B) = CreateSubset(B, C)
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
});

describe("Real Programs", () => {
  // create output folder
  if (saveASTs && !fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir);
  }

  subPaths.forEach((examplePath) => {
    if (!isKeyOf(examplePath, setTheory)) throw Error(examplePath);
    const prog = setTheory[examplePath];
    test(examplePath, () => {
      const { results } = parser.feed(prog);
      sameASTs(results);
      // write to output folder
      if (saveASTs) {
        const exampleName = path.basename(examplePath, ".substance");
        const astPath = path.join(outputDir, exampleName + ".ast.json");
        fs.writeFileSync(astPath, JSON.stringify(results[0]), "utf8");
      }
    });
  });
});
