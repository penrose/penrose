import im from "immutable";
import { describe, expect, test } from "vitest";
import { numOf, numsOf } from "../lib/Utils.js";
import * as ad from "../types/ad.js";
import { C } from "../types/ast.js";
import { DomainEnv } from "../types/domain.js";
import { PenroseError } from "../types/errors.js";
import { State } from "../types/state.js";
import {
  AnonAssign,
  ConstrFn,
  GPIDecl,
  HeaderBlock,
  PathAssign,
  StyProg,
  Vector,
} from "../types/style.js";
import {
  Assignment,
  DepGraph,
  Layer,
  Translation,
} from "../types/styleSemantics.js";
import { SubstanceEnv } from "../types/substance.js";
import { ColorV, FloatV, RGBA, StrV } from "../types/value.js";
import { Result, showError } from "../utils/Error.js";
import Graph from "../utils/Graph.js";
import { GroupGraph } from "../utils/GroupGraph.js";
import { zip2 } from "../utils/Util.js";
import { compileDomain } from "./Domain.js";
import * as S from "./Style.js";
import { compileSubstance } from "./Substance.js";

// TODO: Reorganize and name tests by compiler stage

interface Trio {
  sub: string;
  dsl: string;
  sty: string;
}
// Run the Domain + Substance parsers and checkers to yield the Style compiler's input
export const loadProgs = async ({
  dsl,
  sub,
  sty,
}: Trio): Promise<{
  translation: Translation;
  assignment: Assignment;
  state: State;
  styleAST: StyProg<C>;
  graph: DepGraph;
}> => {
  const throwErr = (e: any): any => {
    throw Error(
      `Expected Style program to work without errors. Got error: ${showError(
        e,
      )}`,
    );
  };
  const domEnv = compileDomain(dsl).unwrapOrElse(throwErr);
  const subEnv = compileSubstance(sub, domEnv).unwrapOrElse(throwErr);
  return (
    await S.compileStyleHelper("styletests", sty, subEnv, domEnv)
  ).unwrapOrElse(throwErr);
};

const canvasPreamble = `canvas {
  width = 800
  height = 700
}
`;

describe("Layering computation", () => {
  // NOTE: again, for each edge (v, w), `v` is __below__ `w`.
  const simpleGroupGraph: GroupGraph = new Graph();
  ["A", "B", "C", "D", "E", "F"].map((x) => {
    simpleGroupGraph.setNode(x, 0);
  });
  test("simple layering: A -> B -> C", () => {
    const partials: Layer[] = [
      { below: "A", above: "B" },
      { below: "B", above: "C" },
    ];
    const { shapeOrdering, warning } = S.computeLayerOrdering(
      ["A", "B", "C"],
      partials,
      simpleGroupGraph,
    );
    expect(shapeOrdering).toEqual(["A", "B", "C"]);
    expect(warning).toBeUndefined();
  });
  test("one cycle: A -> B -> C -> A", () => {
    const partials: Layer[] = [
      { below: "A", above: "B" },
      { below: "B", above: "C" },
      { below: "C", above: "A" },
    ];
    const { shapeOrdering, warning } = S.computeLayerOrdering(
      ["A", "B", "C"],
      partials,
      simpleGroupGraph,
    );
    expect(shapeOrdering).toEqual(["A", "B", "C"]);
    expect(warning).toBeDefined();
    expect(warning?.cycles.length).toEqual(1);
  });
  test("one cycle in tree", () => {
    const partials: Layer[] = [
      { below: "A", above: "B" },
      { below: "A", above: "C" },
      { below: "B", above: "D" },
      { below: "D", above: "E" },
      { below: "E", above: "B" },
      { below: "C", above: "F" },
    ];
    const { shapeOrdering, warning } = S.computeLayerOrdering(
      ["A", "B", "C", "D", "E", "F"],
      partials,
      simpleGroupGraph,
    );
    expect(shapeOrdering).toEqual(["A", "C", "F", "B", "D", "E"]);
    expect(warning).toBeDefined();
    expect(warning?.cycles.length).toEqual(1);
  });
  test("one big cycle", () => {
    const partials: Layer[] = [
      { below: "A", above: "B" },
      { below: "B", above: "D" },
      { below: "C", above: "A" },
      { below: "D", above: "E" },
      { below: "E", above: "C" },
      { below: "C", above: "F" },
    ];
    const { shapeOrdering, warning } = S.computeLayerOrdering(
      ["A", "B", "C", "D", "E", "F"],
      partials,
      simpleGroupGraph,
    );
    expect(shapeOrdering).toEqual(["A", "B", "D", "E", "C", "F"]);
    expect(warning).toBeDefined();
    expect(warning?.cycles.length).toEqual(1);
  });
  test("good group layering", () => {
    const partials: Layer[] = [
      { below: "G1", above: "G2" },
      { below: "A", above: "D" },
      { below: "B", above: "C" },
    ];
    const groupGraph: GroupGraph = new Graph();
    ["A", "B", "C", "D", "G1", "G2"].map((x) => groupGraph.setNode(x, 0));
    groupGraph.setEdge({ i: "G1", j: "A", e: undefined });
    groupGraph.setEdge({ i: "G1", j: "D", e: undefined });
    groupGraph.setEdge({ i: "G2", j: "B", e: undefined });
    groupGraph.setEdge({ i: "G2", j: "C", e: undefined });
    const { shapeOrdering, warning } = S.computeLayerOrdering(
      ["G1", "B", "D", "A", "G2", "C"],
      partials,
      groupGraph,
    );
    // Order is A, D, B, C
    // But, position of G1 and G2 are undetermined
    const aPos = shapeOrdering.indexOf("A");
    const dPos = shapeOrdering.indexOf("D");
    const bPos = shapeOrdering.indexOf("B");
    const cPos = shapeOrdering.indexOf("C");
    expect(aPos < dPos).toBe(true);
    expect(dPos < bPos).toBe(true);
    expect(bPos < cPos).toBe(true);
    expect(warning).toBeUndefined();
  });
  test("bad group layering", () => {
    const partials: Layer[] = [
      { below: "s2", above: "s1" },
      { below: "s2", above: "s3" },
      { below: "s3", above: "s1" },
    ];
    const groupGraph: GroupGraph = new Graph();
    ["s1", "s2", "s3", "g"].map((x) => {
      groupGraph.setNode(x, 0);
    });
    groupGraph.setEdge({ i: "g", j: "s1", e: undefined });
    groupGraph.setEdge({ i: "g", j: "s2", e: undefined });
    const { shapeOrdering, warning } = S.computeLayerOrdering(
      ["g", "s3", "s1", "s2"],
      partials,
      groupGraph,
    );
    expect(warning).toBeDefined();
    expect(warning!.cycles.length).toEqual(1);
  });
});

const colorValMatches = (
  colorPath: string,
  expected: [number, number, number, number],
  translation: Translation,
) => {
  const val = translation.symbols.get(colorPath);
  const rgba = ((val?.contents as ColorV<number>).contents as RGBA<number>)
    .contents;
  zip2(rgba, expected).map(([a, b]) => expect(a).toBeCloseTo(b, 1));
};

describe("Color literals", () => {
  test("color literal values", async () => {
    const { translation } = await loadProgs({
      dsl: "type T",
      sub: `
      T t
      `,
      sty:
        canvasPreamble +
        `
      forall T t {
        t.color1 = #000000
        t.color2 = #FFFFFF
        t.color3 = #a68db8
        t.color4 = #00000000
        t.color5 = #FFFFFF80
        t.color6 = #a68db8FF
        t.color7 = #000
        t.color8 = #FFF
        t.color9 = #fc9
        t.color7 = #0000
        t.color8 = #FFF8
        t.color9 = #fc9F
      }
      `,
    });
    colorValMatches(`\`t\`.color1`, [0, 0, 0, 1], translation);
    colorValMatches(`\`t\`.color2`, [1, 1, 1, 1], translation);
    colorValMatches(`\`t\`.color3`, [0.651, 0.553, 0.722, 1.0], translation);
    colorValMatches(`\`t\`.color4`, [0, 0, 0, 0], translation);
    colorValMatches(`\`t\`.color5`, [1, 1, 1, 0.5], translation);
    colorValMatches(`\`t\`.color6`, [0.651, 0.553, 0.722, 1.0], translation);
    colorValMatches(`\`t\`.color7`, [0, 0, 0, 0], translation);
    colorValMatches(`\`t\`.color8`, [1, 1, 1, 0.5], translation);
    colorValMatches(`\`t\`.color9`, [1, 0.8, 0.6, 1], translation);
  });
});

describe("Staged constraints", () => {
  test("stage spec", async () => {
    const { styleAST: ex2 } = await loadProgs({
      dsl: "type Set",
      sub: `Set A`,
      sty:
        canvasPreamble +
        `
      layout = [ShapeLayout, LabelLayout, Overall]
      `,
    });
    expect(S.getLayoutStages(ex2).unwrapOr(undefined)).toEqual([
      "ShapeLayout",
      "LabelLayout",
      "Overall",
    ]);
  });
  test("varying stages", async () => {
    const { styleAST } = await loadProgs({
      dsl: "type Set",
      sub: `Set A`,
      sty:
        canvasPreamble +
        `
      forall Set X {
        X.icon = Circle {
          center: (? in [Overall, ShapeLayout], ? except [Overall, LabelLayout])
        }
      }
      `,
    });
    const gpiStmt = (styleAST.items[1] as HeaderBlock<C>).block
      .statements[0] as PathAssign<C>;
    const gpiDecl = gpiStmt.value as GPIDecl<C>;
    const rValue = gpiDecl.properties[0].value as Vector<C>;
    const [vary1, vary2] = rValue.contents as [any, any];
    expect(vary1.exclude).toEqual(false);
    expect(vary2.exclude).toEqual(true);
    const stages1 = vary1.stages.map((e: any) => e.value);
    const stages2 = vary2.stages.map((e: any) => e.value);
    expect(stages1).toEqual(["Overall", "ShapeLayout"]);
    expect(stages2).toEqual(["Overall", "LabelLayout"]);
  });
  test("constraint stages", async () => {
    const { styleAST } = await loadProgs({
      dsl: "type Set",
      sub: `Set A`,
      sty:
        canvasPreamble +
        `
      forall Set X {
        X.icon = Circle {}
        X.text = Text {}
        ensure contains(X.icon, X.text) in [ShapeLayout, LabelLayout]
        encourage minimal(X.icon.r) in LabelLayout
      }
      `,
    });
    const ensureStmt = (styleAST.items[1] as HeaderBlock<C>).block
      .statements[2] as AnonAssign<C>;
    const ensureExpr = ensureStmt.contents as ConstrFn<C>;
    const stages1 = ensureExpr.stages.map((e) => e.value);
    expect(stages1).toEqual(["ShapeLayout", "LabelLayout"]);
    const encourageStmt = (styleAST.items[1] as HeaderBlock<C>).block
      .statements[3] as AnonAssign<C>;
    const encourageExpr = encourageStmt.contents as ConstrFn<C>;
    const stages2 = encourageExpr.stages.map((e) => e.value);
    expect(stages2).toEqual(["LabelLayout"]);
  });
});

describe("Compiler", () => {
  test("Label insertion", async () => {
    const { assignment } = await loadProgs({
      dsl: "type Set",
      sub: `
      Set A, B, C
      AutoLabel All
      Label A $aaa$
      NoLabel B
      `,
      sty: canvasPreamble + ``,
    });
    const { substances } = assignment;
    for (const [name, label] of [
      ["A", "aaa"],
      ["B", ""],
      ["C", "C"],
    ]) {
      const v = substances.get(name)?.get("label");
      if (v?.tag === "OtherSource" && v?.expr.tag === "StringLit") {
        expect(v.expr.contents).toBe(label);
      }
    }
  });

  // COMBAK: StyleTestData is deprecated. Make the data in the test file later (@hypotext).
  // // Each possible substitution should be full WRT its selector
  // test("substitution: S.fullSubst true", () => {
  //   for (let i = 0; i < selEnvs.length; i++) {
  //     for (let j = 0; j < possibleSubsts[i].length; j++) {
  //       expect(S.fullSubst(selEnvs[i], possibleSubsts[i][j] as Subst)).toEqual(
  //         true
  //       );
  //     }
  //   }
  // });

  // COMBAK: StyleTestData is deprecated. Make the data in the test file later (@hypotext).
  // test("substitution: S.fullSubst false", () => {
  //   // Namespace shouldn't have matches
  //   const ps0: Subst = { test: "A" };
  //   expect(S.fullSubst(selEnvs[0], ps0)).toEqual(false);

  //   // Selector should have real substitution
  //   const ps1 = { v: "x1", U: "X" }; // missing "w" match
  //   expect(S.fullSubst(selEnvs[6], ps1)).toEqual(false);
  // });

  test("substitution: S.uniqueKeysAndVals", () => {
    expect(
      S.uniqueKeysAndVals({
        a: { tag: "SubstanceVar", name: "v" },
        c: { tag: "SubstanceVar", name: "z" },
      }),
    ).toEqual(true);
    expect(
      S.uniqueKeysAndVals({
        a: { tag: "SubstanceVar", name: "V" },
        c: { tag: "SubstanceVar", name: "V" },
      }),
    ).toEqual(false);
    expect(
      S.uniqueKeysAndVals({
        a: {
          tag: "SubstanceLiteral",
          contents: { tag: "SubstanceNumber", contents: 1 },
        },
        c: { tag: "SubstanceVar", name: "z" },
      }),
    ).toEqual(true);
    expect(
      S.uniqueKeysAndVals({
        a: {
          tag: "SubstanceLiteral",
          contents: { tag: "SubstanceString", contents: "Hello world" },
        },
        c: {
          tag: "SubstanceLiteral",
          contents: { tag: "SubstanceString", contents: "Hello world" },
        },
      }),
    ).toEqual(false);
  });

  // COMBAK: StyleTestData is deprecated. Make the data in the test file later (@hypotext).
  // // For the 6th selector in the LA Style program, substituting in this substitution into the relational expressions yields the correct result (where all vars are unique)
  // test("substitute unique vars in selector", () => {
  //   const subst: Subst = { v: "x1", U: "X", w: "x2" };
  //   const rels: RelationPattern[] = selEnvs[6].header.contents.where.contents; // This is selector #6 in the LA Style program
  //   // `rels` stringifies to this: `["In(v, U)", "Unit(v)", "Orthogonal(v, w)"]`
  //   const relsSubStr = rels
  //     .map((rel) => S.substituteRel(subst, rel))
  //     .map(S.ppRel);
  //   const answers = ["In(x1, X)", "Unit(x1)", "Orthogonal(x1, x2)"];

  //   for (const [res, expected] of _.zip(relsSubStr, answers)) {
  //     expect(res).toEqual(expected);
  //   }
  // });

  // COMBAK: StyleTestData is deprecated. Make the data in the test file later (@hypotext).
  // // For the 6th selector in the LA Style program, substituting in this substitution into the relational expressions yields the correct result (where two vars are non-unique, `x2`)
  // test("substitute non-unique vars in selector", () => {
  //   const subst: Subst = { v: "x2", U: "X", w: "x2" };
  //   const rels: RelationPattern[] = selEnvs[6].header.contents.where.contents; // This is selector #6 in the LA Style program
  //   // `rels` stringifies to this: `["In(v, U)", "Unit(v)", "Orthogonal(v, w)"]`
  //   const relsSubStr = rels
  //     .map((rel) => S.substituteRel(subst, rel))
  //     .map(S.ppRel);
  //   const answers = ["In(x2, X)", "Unit(x2)", "Orthogonal(x2, x2)"];

  //   for (const [res, expected] of _.zip(relsSubStr, answers)) {
  //     expect(res).toEqual(expected);
  //   }
  // });

  // COMBAK: StyleTestData is deprecated. Make the data in the test file later (@hypotext).
  // // Compiler finds the right substitutions for LA Style program
  // // Note that this doesn't test subtypes
  // test("finds the right substitutions for LA Style program", () => {
  //   // This code is cleaned up from `S.compileStyle`; runs the beginning of compiler checking from scratch
  //   const triple: [string, string, string] = [
  //     "linear-algebra-domain/linear-algebra.domain",
  //     "linear-algebra-domain/twoVectorsPerp-unsugared.substance",
  //     "linear-algebra-domain/linear-algebra-paper-simple.style",
  //   ];

  //   const [varEnv, subEnv, subProg, styProgInit]: [
  //     Env,
  //     SubstanceEnv,
  //     SubProg,
  //     StyProg
  //   ] = loadProgs(loadFiles(triple) as [string, string, string]);

  //   const selEnvs = S.checkSelsAndMakeEnv(varEnv, styProgInit.blocks);

  //   const selErrs: StyleError[] = _.flatMap(selEnvs, (e) =>
  //     e.warnings.concat(e.errors)
  //   );

  //   if (selErrs.length > 0) {
  //     const err = `Could not compile. Error(s) in Style while checking selectors`;
  //     console.log([err].concat(selErrs.map((e) => showError(e))));
  //     throw Error();
  //   }

  //   if (subss.length !== correctSubsts.length) {
  //     throw Error();
  //   }

  //   for (const [res, expected] of _.zip(subss, correctSubsts)) {
  //     expect(res).toEqual(expected);
  //   }
  // });

  test("Correct Style programs", () => {
    const dsl = "type Object";
    const sub = "Object o";
    // TODO: Name these programs
    const stys = [
      // These were previously mostly to test setting shape properties as
      // vectors or accesspaths, but the ability to override accesspaths was
      // removed in the compiler rewrite (see the comment in the "first Style
      // compiler pass" section of `types/styleSemantics`)
      `forall Object o {
    shape o.shape = Line {}
    -- o.shape.start[0] = 0.
}
`,
      `forall Object o {
    shape o.shape = Line {
        start: (0., ?[123.456])
    }
}`,
      `forall Object o {
    shape o.shape = Line {
          start: (?, ?)
    }
    -- o.shape.start[0] = 0.
}`,
      `forall Object o {
    o.y = ?
    shape o.shape = Line {
        start: (0., o.y)
    }
}`,
      // Set field
      `forall Object o {
       o.f = (?, ?)
       -- o.f[0] = 0.
       o.shape = Circle {}
}`,
      `forall Object o {
        o.a = ? [ 98765.4]
        o.b = ?
        ensure o.a > o.b
        ensure o.a < o.b
        ensure o.a == o.b
      }`,
      `forall Object o {
        o.a = Circle {}
        o.b = Circle {}
        o.c = Rectangle {}
        o.g = Group {
          shapes: [o.a, o.b]
          clipPath: noClip()
        }
        override o.g.clipPath = clip(o.c)
      }`,
    ];
    stys.forEach((sty: string) => {
      expect(
        async () => await loadProgs({ dsl, sub, sty: canvasPreamble + sty }),
      ).not.toThrowError();
    });
  });

  // TODO: There are no tests directly for the substitution application part of the compiler, though I guess you could walk the AST (making the substitution-application code more generic to do so) and check that there are no Style variables anywhere? Except for, I guess, namespace names?
  describe("Symmetric predicates", () => {
    test("non-symmetric predicate should not match", async () => {
      const dsl = `type Atom
      type Hydrogen <: Atom
      type Oxygen <: Atom
      predicate Bond(Atom, Atom)`;
      const sub = `Hydrogen H
      Oxygen O
      Bond(H, O)`;
      const sty =
        canvasPreamble +
        `forall Hydrogen h; Oxygen o
      where Bond(o, h) {
        myShape = Text {
          string: "Bond!"
        }
      }`;
      const { state } = await loadProgs({ dsl, sub, sty });
      expect(state.shapes.length).toEqual(0);
    });
    test("symmetric predicate should match 1", async () => {
      const dsl = `type Atom
      type Hydrogen <: Atom
      type Oxygen <: Atom
      symmetric predicate Bond(Atom, Atom)`;
      const sub = `Hydrogen H
      Oxygen O
      Bond(H, O)`;
      const sty =
        canvasPreamble +
        `forall Hydrogen h; Oxygen o
      where Bond(o, h) {
        myShape = Text {
          string: "Bond!"
        }
      }`;
      const { state } = await loadProgs({ dsl, sub, sty });
      expect(state.shapes.length).toBeGreaterThan(0);
    });
    test("symmetric predicate should match 2", async () => {
      const dsl = `type Set
      symmetric predicate Equal(Set, Set)`;
      const sub = `Set A, B, C
      Equal(A, B)
      Equal(A, C)`;
      const sty =
        canvasPreamble +
        `forall Set x, y, z
      where Equal(x, y); Equal(y, z) {
        myShape = Text {
          string: "Equality!"
        }
      }`;
      const { state } = await loadProgs({ dsl, sub, sty });
      expect(state.shapes.length).toBeGreaterThan(0);
    });
  });

  describe("number of matchings", () => {
    test("no double matching, non-symmetric", async () => {
      const dsl = `type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
predicate Bond(Atom, Atom)`;
      const sub = `Hydrogen H1, H2
      Oxygen O
      Bond( O, H1 )
      Bond( O, H2 )`;
      const sty =
        canvasPreamble +
        `forall Oxygen o; Hydrogen h1; Hydrogen h2
        where Bond(o,h1); Bond(o,h2) {
            myText = Text {
                string: "Water!"
                fillColor: rgba(0, 0, 0, 255)
            }
        }`;

      const { state } = await loadProgs({ dsl, sub, sty });
      expect(state.shapes.length).toEqual(1);
    });

    test("no double matching, symmetric", async () => {
      const dsl = `type Atom
      symmetric predicate Bond(Atom, Atom)`;
      const sub = `Atom A1, A2
      Bond( A1, A2 )`;
      const sty =
        canvasPreamble +
        `forall Atom a1; Atom a2
        where Bond(a1, a2) {
            myText = Text {
                string: "Bond"
            }
        }`;
      const { state } = await loadProgs({ dsl, sub, sty });
      expect(state.shapes.length).toEqual(1);
    });

    test("extra variables not in relations", async () => {
      const dsl = `type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
predicate Bond(Atom, Atom)`;
      const sub = `Hydrogen H1, H2
      Oxygen O
      Bond( O, H1 )
      Bond( O, H2 )
      Hydrogen H3, H4`;
      const sty =
        canvasPreamble +
        `forall Oxygen o; Hydrogen h1; Hydrogen h2; Hydrogen h3
        where Bond(o,h1); Bond(o,h2) {
            myText = Text {
                string: "Water!"
                fillColor: rgba(0, 0, 0, 255)
            }
        }`;
      const { state } = await loadProgs({ dsl, sub, sty });
      expect(state.shapes.length).toEqual(2);
    });

    test("pure selector, no relations", async () => {
      const dsl = `type Atom`;
      const sub = `Atom A1, A2`;
      const sty =
        canvasPreamble +
        `forall Atom a1; Atom a2 {
            myText = Text {
                string: "TwoAtoms!"
                fillColor: rgba(0, 0, 0, 255)
            }
        }`;
      const { state } = await loadProgs({ dsl, sub, sty });
      expect(state.shapes.length).toEqual(1);
    });

    test("repeatable", async () => {
      const dsl = "type T\npredicate P(T, T, T)";
      const sub =
        "T t1, t2, t3\nP(t1, t2, t3)\nP(t1, t1, t3)\nP(t2,t2,t3)\nP(t1,t3,t1)";
      const sty1 =
        canvasPreamble +
        `forall repeatable T t1; T t2 {
          Circle {}
        }`;

      const res1 = await loadProgs({ dsl, sub, sty: sty1 });
      expect(res1.state.shapes.length).toEqual(6);

      const sty2 =
        canvasPreamble +
        `forall repeatable T t1; T t2; T t3
        where P(t1, t2, t3) {
          Circle {}
        }`;
      const res2 = await loadProgs({ dsl, sub, sty: sty2 });
      expect(res2.state.shapes.length).toEqual(4);
    });
  });

  describe("predicate alias", () => {
    test("general predicate alias with symmetry", async () => {
      const dsl = `type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
symmetric predicate Bond(Atom, Atom)
`;
      const sub = `Hydrogen H1, H2
Oxygen O
Bond(O, H1)
Bond(O, H2)`;
      const sty =
        canvasPreamble +
        `
    forall Oxygen o; Hydrogen h
    where Bond(h, o) as b {
        b.shape = Line {
        }
    }
    `;
      const { state } = await loadProgs({ dsl, sub, sty });
      expect(state.shapes.length).toEqual(2);
    });
    test("correct style programs with predicate aliasing", async () => {
      const dsl = "type Set \n predicate Subset(Set, Set)";
      const sub = "Set A\nSet B\nSet C\nSubset(B, A)\nSubset(C, B)";

      const sty =
        canvasPreamble +
        `forall Set a; Set b where Subset(a,b) as foo {
          foo.icon = Rectangle{}
        }
        forall Set u; Set v where Subset(u,v) as bar {
          bar.icon2 = Ellipse{}
        }
        `;

      const { state } = await loadProgs({ dsl, sub, sty });
      expect(state.shapes.length).toEqual(4);
    });
  });
  // Test errors
  const PRINT_ERRORS = false;

  const expectErrorOf = (
    result: Result<State, PenroseError>,
    errorType: string,
  ) => {
    if (result.isErr) {
      const res: PenroseError = result.error;
      if (res.errorType !== "StyleError") {
        throw Error(
          `Error ${errorType} was supposed to occur. Got a non-Style error '${res.errorType}'.`,
        );
      }

      if (res.tag !== "StyleErrorList") {
        throw Error(
          `Error ${errorType} was supposed to occur. Did not receive a Style list. Got ${res.tag}.`,
        );
      }

      if (PRINT_ERRORS) {
        console.log(result.error);
      }

      expect(res.errors[0].tag).toBe(errorType);
    } else {
      const { warnings } = result.value;

      if (warnings.length === 0) {
        throw Error(`Error ${errorType} was supposed to occur.`);
      }

      if (PRINT_ERRORS) {
        console.log(warnings);
      }

      expect(warnings[0].tag).toBe(errorType);
    }
  };

  describe("Expected Style errors", () => {
    const subProg = `Set A, B
Subset(B, A)
AutoLabel All `;

    const domainProg = `type Set
type Point

function Union(Set a, Set b) -> Set

predicate Subset(Set s1, Set s2)
`;

    // We test variations on this Style program
    // const styPath = "set-theory-domain/euler.style";

    const domRes: Result<DomainEnv, PenroseError> = compileDomain(domainProg);

    if (domRes.isErr) {
      throw new Error("Domain compilation should not fail");
    }

    const subRes: Result<SubstanceEnv, PenroseError> = compileSubstance(
      subProg,
      domRes.value,
    );

    if (subRes.isErr) {
      throw new Error("Substance compilation should not fail");
    }

    const testStyProgForError = async (styProg: string, errorType: string) => {
      let preamble = errorType.startsWith("Canvas") ? "" : canvasPreamble;
      const styRes: Result<State, PenroseError> = await S.compileStyle(
        "Style compiler errors test seed",
        preamble + styProg,
        [],
        subRes.value,
        domRes.value,
      );
      expectErrorOf(styRes, errorType);
    };

    const errorStyProgs = {
      // ------ Generic errors
      InvalidColorLiteral: [
        `forall Set x {
          x.color = #12777733aa
       }`,
      ],
      // ------ Selector errors (from Substance)
      SelectorVarMultipleDecl: [`forall Set x; Set x { }`],
      SelectorFieldNotSupported: [`forall Set x where x has randomfield { }`],

      // COMBAK: Style doesn't throw parse error if the program is just "forall Point `A`"... instead it fails inside compileStyle with an undefined selector environment
      //SelectorDeclTypeMismatch: [`forall Point \`A\` { }`],
      // ^ This should not be an error.

      // SelectorRelTypeMismatch: [
      //   `forall Point x; Set y; Set z
      // where x := Union(y, z) { } `,
      // ],

      TaggedSubstanceError: [
        `forall Set x; Point y
where Subset(y, x) { }`,
        `forall Setfhjh x { }`,
        `forall Point x, y where Midpointdfsdfds(x, y) { }`,
        `forall Set a where Subset(a, B) {}`,
      ],

      // ---------- Block static errors

      InvalidGPITypeError: [`forall Set x { x.icon = Circl { } }`],

      // Have to do a nested search in expressions for this
      InvalidFunctionNameError: [
        `forall Set x { x.icon = Circle { r: ksajfksdafksfh(0.0, "hi") } }`,
        `forall Set x { x.icon = Circle { r: get(0.0, sjkfhsdk("hi")) } }`,
        `forall Set x { x.icon = Circle { r: wjhkej(0.0, sjkfhsdk("hi")) } }`,
      ],

      InvalidObjectiveNameError: [`forall Set x { encourage sjdhfksha(0.0) }`],

      InvalidConstraintNameError: [`forall Set x { ensure jahfkjdhf(0.0) }`],

      // ------- Compilation errors

      MissingShapeError: [
        `forall Set x { x.icon = Circle { }
delete x.z.p }`,
        `forall Set x {
    x.icon.r = 0.0
}`,
      ],

      NotShapeError: [
        `forall Set x { x.z = 0.0
delete x.z.p }`,
        `forall Set x {
    x.icon = "hello"
    x.icon.r = 0.0
}`,
      ],

      // COMBAK: This doesn't catch the error
      // COMBAK: Style appears to throw parse error if line ends with a trailing space
      // COMBAK: Why is this a parse error??
      // COMBAK: Test the instance in `insertExpr` as well as in `compileStyle`
      //       CircularPathAlias: [`forall Set x { x.icon = Circle { }
      // x.icon.center = x.icon.center }`],

      NoopDeleteWarning: [`forall Set x { delete x.z }`],
      AssignAccessError: [
        `forall Set x {
         x.icon = Circle {
           center: (0.0, 0.0)
         }
         delete x.icon[0] }`,
      ],

      ImplicitOverrideWarning: [
        `forall Set x {
           x.z = 1.0
           x.z = 2.0
}`,
        `forall Set x {
         x.icon = Circle {
           center: (0.0, 0.0)
         }
           x.icon.center = (2.0, 0.0)
}`,
      ],

      CyclicAssignmentError: [
        `forall Set x {
          x.icon = Circle { }
        }

        forall Set x; Set y where Subset(x, y) {
          override y.r = x.r + y.r
        }`,
      ],

      // TODO(errors): check multiple errors

      // TODO(errors): This throws too early, gives InsertedPathWithoutOverrideError -- correctly throws error but may be misleading

      // NonexistentNameError:
      //   [`forall Set x { A.z = 0. }`],

      MissingPathError: [
        `forall Set x { x.icon = Circle { r: x.r } }`,
        `forall Set x {
         x.z = x.c.p
       }`,
        `forall Set x {
          x.icon = Circle {
           r: 9.
           center: (x.icon.z, 0.0)
         }
       }`,
        `forall Set x {
           x.z = 1.0
           x.y = x.z.p
}`,
        `forall Set x {
          layer AAA above BBB
        }`,
      ],
      CanvasNonexistentDimsError: [
        `foo {
  bar = 1.0
}`,
        `canvas {
  height = 100
}`,
        `canvas {
  width = Circle {}
  height = 100
}`,
        `canvas {
  width = ?[300]
  height = 100
}`,
        `canvas {
  width = (1.0, 1.0)
  height = 100
}`,
        `canvas {
  width = 100
}`,
        `canvas {
  width = 100
  height = Circle {}
}`,
        `canvas {
  width = 100
  height = ?
}`,
        `canvas {
  width = 100
  height = (1.0, 1.0)
}`,
      ],
      SelectorAliasNamingError: [
        `forall Set a; Set b
        where Subset(a, b) as a {}`,
        `forall Set a; Set b
        where Subset(a, b) as Set {}`,
        `forall Set a; Set b
        where Subset(a, b) as Subset {}`,
      ],
      BadShapeParamTypeError: [
        `forall Set a {
          a.sh = Circle {
            r: "a string"
          }
        }`,
        `forall Set a {
          a.sh = Circle {
            ptProp: (1, 2, 3)
          }
        }`,
        `forall Set a {
          a.sh = Group {
            shapes: []
            clipPath: 12345
          }
        }
        `,
      ],
      BadArgumentTypeError: [
        `forall Set a {
          x = circumradius([1, 2, 3], [4, 5, 6], [7, 8, 9])
        }`,
        `forall Set a {
          x = cubicCurveFromPoints("hello", [(1, 2), (3, 4), (5, 6)])
        }`,
        `forall Set a {
          x = cubicCurveFromPoints("closed", [(1, 2, 3), (3, 4, 5), (5, 6, 7), (6, 7, 8)])
        }`,
        `forall Set a {
          c = Circle {}
          encourage isRegular(c)
        }`,
        `forall Set a {
          x = clip(123)
        }
        `,
      ],
      MissingArgumentError: [
        `forall Set a {
          a.s = Circle {}
          ensure contains(a.s)
        }`,
        `forall Set a {
          ensure disjoint()
        }`,
        `
        forall Set a {
          x = clip()
        }
        `,
      ],
      TooManyArgumentsError: [
        `forall Set a {
          a.s = Circle {}
          ensure contains(a.s, a.s, 1, 2, 3)
        }`,
        `forall Set a {
          a.s = Circle {}
          x = noClip(a.s)
        }`,
      ],
      FunctionInternalError: [
        `forall Set a {
          x = dot([1, 2, 3], [4, 5])
        }`,
        `forall Set a {
          x = Group {}
          y = Group {
            shapes: []
            clipPath: clip(x)
          }
        }`,
      ],
      RedeclareNamespaceError: [
        `Colors {
          red = #f00
        }
        Colors {
          red = #e00
        }
        `,
      ],
      NotSubstanceCollectionError: [
        `forall Set a {
          a.c = 10
          x = listof c from a
        }`,
        `collect Set a into aa foreach Set b {
          x = listof c from b
        }`,
        `collect Set a into aa foreach Set b {
          x = listof c from x
        }`,
      ],
      LayerOnNonShapesError: [
        `block {
          x = 100
          y = 200
          layer x above y
        }`,
      ],
      BadElementError: [
        `forall Set a {
          a.c = 10
        }
        forall Set \`B\` {
          override \`B\`.c = "hello"
        }
        collect Set a into aa {
          x = listof c from aa
        }`,
      ],
      UndeclaredSubVarError: [
        `forall Set \`B\` {
           \`B\`.x = 100 
         }
           forall Set a {
           a.x = \`B\`.x
         }`,
      ],
      PathToSubstanceError: [
        `forall Set x {
          v = x
        }`,
      ],
      PathToCollectionError: [
        `collect Set s into ss {
          x = ss
        }`,
      ],
      PathToNamespaceError: [
        `ns {
          x = 100
        }
        forall Set x {
          y = ns
        }`,
      ],
      CollectionMemberAccessError: [
        `collect Set s into ss {
          x = ss.r
        }`,
      ],
      UnindexableItemError: [
        `forall Set x {
          x.icon = Circle {}
          y = x.icon[1]
        }`,
      ],
      // TODO: this test should _not_ fail, but it's failing because we are skipping `OptEval` checks for access paths
      //       InvalidAccessPathError: [
      //         `forall Set x {
      //            x.z = 1.0
      //            x.y = x.z[0]
      // }`,
      //       ],

      // ---------- Runtime errors (insertExpr)

      // COMBAK / TODO(errors): Test this more thoroughly
      // COMBAK / NOTE: runtime errors aren't caught if an expr isn't evaluated, since we don't have statics. Test these separately.

      //       RuntimeValueTypeError: [
      //         `forall Set x {
      //          x.icon = Circle {
      //            center: (0.0, 0.0)
      //          }
      //            x.icon.center[0] = "hello"
      //            x.icon.center[1] = x.icon.center[0]
      // }`]
    };

    // ---------- Errors that should maybe be modeled + tested

    // COMBAK: Should this have a warning/error? Currently doesn't
    // `forall Set x { x.icon = Circle { }; delete x.icon.z }

    // COMBAK: These both cause a weird crash that's not caught
    // override x.icon = x.icon
    // ensure contains(x.icon, x.text2)

    // forall Set x { x.icon = Circle { }
    // x.icon2 = x.icon
    // x.icon2.strokeWidth = 5.0
    // -- COMBAK: This line does not appear to have worked
    // delete x.icon2.strokeWidth
    // }

    test("that each program yields its error type", async () => {
      for (const [errorType, styProgs] of Object.entries(errorStyProgs)) {
        for (const styProg of styProgs) {
          // TODO(error): improve this so it becomes individual tests, using the framework
          // console.log("testing", errorType);
          await testStyProgForError(styProg, errorType);
        }
      }
    });
  });

  describe("faster matching", () => {
    test("multiple predicates", async () => {
      const sub = `
      MySet X, Y
 OtherType Z

 MyPred(Z, X, Y)
 MyOtherPred(X, Y)`;
      const dsl = `
     type MySet
 type OtherType

 predicate MyPred(OtherType, MySet, MySet)
 predicate MyOtherPred(MySet, MySet)`;

      const sty =
        canvasPreamble +
        `
     forall MySet X; MySet Y; OtherType Z
 where MyPred(Z, X, Y); MyOtherPred(X, Y) {
     theCircle = Circle {
         r: 20
     }
 }`;

      const { state } = await loadProgs({ dsl, sub, sty });
      expect(state.shapes.length).toEqual(1);
    });
    test("many declaration matches with only one relational match", async () => {
      const sub = `
      T t1, t2, t3, t4, t5, t6, t7, t8
      S s := f( t1, t2, t3, t4, t5, t6, t7, t8 )`;
      const dsl = `
      -- minimal.domain
      type S
      type T
      constructor f( T t1, T t2, T t3, T t4, T t5, T t6, T t7, T t8 ) -> S`;

      const sty =
        canvasPreamble +
        `
        forall S s; T t1; T t2; T t3; T t4; T t5; T t6; T t7; T t8
        where s := f( t1, t2, t3, t4, t5, t6, t7, t8 ) {
           s.shape = Circle {
              center: (0,0)
              r: 10.0
           }
        }`;
      const { state } = await loadProgs({ dsl, sub, sty });
      expect(state.shapes.length).toEqual(1);
    });
  });
  describe("match metadata", () => {
    test("match total", async () => {
      const dsl = "type MyType\n";
      const sty =
        canvasPreamble +
        `forall MyType t {
  t.shape = Circle {
    ptProp: match_total
  }
}`;
      const sub = "MyType t1, t2, t3\n";
      const { state } = await loadProgs({ dsl, sub, sty });
      expect(
        state.shapes.every((shape) => {
          const val = shape.passthrough.get("ptProp");
          if (val && val.tag === "FloatV") {
            const v = val.contents;
            return v === 3;
          } else {
            return false;
          }
        }),
      ).toEqual(true);
    });

    test("match id", async () => {
      const dsl = "type MyType\n";
      const sty =
        canvasPreamble +
        `forall MyType t {
  t.shape = Circle {
    ptProp: match_id
  }
}`;
      const sub = "MyType t1, t2, t3\n";

      const { state } = await loadProgs({ dsl, sub, sty });

      // Require that the match_id's are exactly [1, 2, 3]
      expect(
        im.Set(
          state.shapes.map((shape) => {
            const v = shape.passthrough.get("ptProp");
            if (v && v.tag === "FloatV") {
              return v.contents;
            } else {
              throw Error("Should be a FloatV");
            }
          }),
        ),
      ).toEqual(im.Set([1, 2, 3]));
    });
  });
  describe("group shapes", () => {
    test("simple group", async () => {
      const dsl = "type T\n";
      const sty =
        canvasPreamble +
        `
      forall T t {
        t.s1 = Circle {}
        t.s2 = Rectangle {}
        t.g = Group {
          shapes: [t.s1, t.s2]
        }
        t.s3 = Text {}
        t.g2 = Group {
          shapes: [t.s3, t.g]
        }
      }\n
      `;
      const sub = "T t\n";
      const { state } = await loadProgs({ dsl, sub, sty });
      expect(state.shapes.length).toEqual(1);
    });
  });

  describe("group graph", () => {
    test("cyclic group graph", () => {
      const groupGraph: GroupGraph = new Graph();
      ["A", "B", "C"].map((x) => {
        groupGraph.setNode(x, 0);
      });
      groupGraph.setEdge({ i: "A", j: "B", e: undefined });
      groupGraph.setEdge({ i: "B", j: "C", e: undefined });
      groupGraph.setEdge({ i: "C", j: "A", e: undefined });

      const warnings = S.checkGroupGraph(groupGraph);
      expect(warnings.length).toEqual(1);
      expect(warnings[0].tag).toEqual("GroupCycleWarning");
    });
    test("shape belongs to multiple groups", () => {
      const groupGraph: GroupGraph = new Graph();
      ["X", "A", "B"].map((x) => {
        groupGraph.setNode(x, 0);
      });
      groupGraph.setEdge({ i: "A", j: "X", e: undefined });
      groupGraph.setEdge({ i: "B", j: "X", e: undefined });
      const warnings = S.checkGroupGraph(groupGraph);
      expect(warnings.length).toEqual(1);
      expect(warnings[0].tag).toEqual("ShapeBelongsToMultipleGroups");
    });
  });

  describe("Global namespace", () => {
    test("namespace override", async () => {
      const { translation } = await loadProgs({
        dsl: ``,
        sub: ``,
        sty:
          canvasPreamble +
          `
          Colors {
            color red = #e00
            color green = #0e0
          }
          OverrideColors {
            override Colors.red = #f00
            override Colors.green = #0f0
            Colors.blue = #00f
          }
          `,
      });
      colorValMatches(`Colors.red`, [1, 0, 0, 1], translation);
      colorValMatches(`Colors.green`, [0, 1, 0, 1], translation);
      colorValMatches(`Colors.blue`, [0, 0, 1, 1], translation);
    });
  });

  describe("collector", () => {
    const dsl = "type T \n type U \n predicate P(T, U)";
    const sub =
      "T t1, t2, t3 \n U u1, u2 \n P(t1, u1) \n P(t2, u1) \n P(t3, u2) \n AutoLabel All";
    test("pure collection", async () => {
      const sty =
        canvasPreamble +
        `
        forall T t {
          t.value = 1
        }
        collect T t into ts {
          Circle {
            r: numberof ts
          }
        }
      `;
      const { state } = await loadProgs({ dsl, sub, sty });
      const sh = state.shapes[0];
      if (sh.shapeType === "Circle") {
        expect(sh.r.contents).toBeCloseTo(3);
      } else {
        throw new Error("Bad shape type");
      }
    });
    test("pure collection with foreach", async () => {
      const sty =
        canvasPreamble +
        `
        forall T t {
          t.value = 1
        }
        collect T t into ts
        where P(t, u)
        foreach U u {
          Circle {
            r: count(listof value from ts)
          }
        }
      `;
      const { state } = await loadProgs({ dsl, sub, sty });
      const sh0 = state.shapes[0],
        sh1 = state.shapes[1];
      if (sh0.shapeType === "Circle" && sh1.shapeType === "Circle") {
        const counts = [sh0.r.contents, sh1.r.contents];
        expect(counts.sort()).toEqual([1, 2]);
      } else {
        throw new Error("Bad shape type");
      }
    });
  });

  describe("gather dependencies", () => {
    test("indexing", async () => {
      const dsl = "type T";
      const sub = "T t";
      const sty =
        canvasPreamble +
        `
        forall T t {
          t.vals = [1, 2, 3, 4, 5, 6]
          t.val = t.vals[match_id]
        
          Circle {
            r: t.val
          }
        }
      `;

      // This problem would have failed compilation when indexing is not handled correctly
      // And this would fail:
      const { graph } = await loadProgs({ dsl, sub, sty });
      expect(graph.parents("`t`.val").sort()).toEqual(
        ["`t`.vals", "1:0.match_id"].sort(),
      );
    });
  });

  test("Indexing", async () => {
    const dsl = "type T";
    const sub = "T t";
    const sty =
      canvasPreamble +
      `
      forall T t {
        mat = [(1, 2, 3), (4, 5, 6), (7, 8, 9)]
        t.row = mat[2]
      }
    `;

    const { translation } = await loadProgs({ dsl, sub, sty });
    const rowVal = translation.symbols.get("`t`.row");
    expect(rowVal !== undefined).toBe(true);
    if (rowVal !== undefined) {
      expect(rowVal.tag).toEqual("Val");
      if (rowVal.tag === "Val") {
        expect(rowVal.contents.tag).toEqual("VectorV");
        if (rowVal.contents.tag === "VectorV") {
          expect(numsOf(rowVal.contents.contents)).toEqual([7, 8, 9]);
        }
      }
    }
  });

  describe("selector literals", () => {
    test("declared styvars refers to numbers", async () => {
      const dsl = "predicate Even(Number n)";
      const sub = `Even(-4)
        Even(-2)
        Even(0)
        Even(2)
        Even(4)`;
      const sty =
        canvasPreamble +
        `
        forall Number n
        where Even(n) {
          n.sh = Circle {
            r: n
          }
        }
      `;

      const { translation } = await loadProgs({ dsl, sub, sty });
      [-4, -2, 0, 2, 4].forEach((num) => {
        const subName = `{n${num}}`;
        // ensure each shape exists
        expect(translation.symbols.get(`\`${subName}\`.sh`)).toBeDefined();

        // ensure each shape has the right radius
        expect(
          (
            translation.symbols.get(`\`${subName}\`.sh.r`)
              ?.contents as FloatV<ad.Num>
          ).contents,
        ).toEqual(num);
      });
    });
    test("undeclared styvars that refers to numbers", async () => {
      const dsl = `type Set
        predicate Has(Set s, Number n)`;
      const sub = `Set s
        Has(s, -1.234)
        Has(s, 3)
        Has(s, 5.678)`;
      const sty =
        canvasPreamble +
        `
        forall Set s
        where Has(s, n) {
          n.sh = Circle {
            r: n
          }
        }
      `;
      // This is fine -- the `n` in `n.sh` would be translated to `{n...}`.sh, because resolveLhsPath handles this.

      const { translation } = await loadProgs({ dsl, sub, sty });
      [-1.234, 3, 5.678].forEach((num) => {
        const subName = `{n${num}}`;
        // ensure each shape exists
        expect(translation.symbols.get(`\`${subName}\`.sh`)).toBeDefined();

        // ensure each shape has the right radius
        expect(
          (
            translation.symbols.get(`\`${subName}\`.sh.r`)
              ?.contents as FloatV<ad.Num>
          ).contents,
        ).toEqual(num);
      });
    });

    test("literals in selector", async () => {
      const dsl = `type Set
        predicate Has(Set s, String str)`;
      const sub = `Set s1, s2
        Has(s1, "Never Gonna Give You Up")
        Has(s2, "Never Gonna Let You Down")`;
      const sty =
        canvasPreamble +
        `
        forall Set s
        where Has(s, "Never Gonna Let You Down") {
          s.t = Text {
            string: nameof s
          }
        }
        `;
      const { translation, state } = await loadProgs({ dsl, sub, sty });

      expect(state.shapes.length).toEqual(1);

      expect(
        (translation.symbols.get(`\`s2\`.t.string`)!.contents as StrV)
          .contents === "s2",
      );
    });

    test("literals in collectors", async () => {
      const dsl = `type Set
        predicate Has(Set s, Number n)`;
      const sub = `Set s1, s2
        Has(s1, 1)
        Has(s1, 5)
        Has(s1, 100)
        Has(s2, -1)
        Has(s2, -5)
        Has(s2, -100)`;
      const sty =
        canvasPreamble +
        `
        collect Number n into ns
        where Has(s, n)
        foreach Set s {
          s.mean = average(ns)
        }
        `;
      const { translation } = await loadProgs({ dsl, sub, sty });
      expect(
        numOf(
          (translation.symbols.get("`s1`.mean")!.contents as FloatV<ad.Num>)
            .contents,
        ),
      ).toEqual((1 + 5 + 100) / 3.0);
      expect(
        numOf(
          (translation.symbols.get("`s2`.mean")!.contents as FloatV<ad.Num>)
            .contents,
        ),
      ).toEqual(-(1 + 5 + 100) / 3.0);
    });
  });
});
