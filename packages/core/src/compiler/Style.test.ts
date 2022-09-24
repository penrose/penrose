// Must be run from penrose-web for loading files

import { examples } from "@penrose/examples";
import * as S from "compiler/Style";
import { buildAssignment } from "compiler/Style";
import { compileSubstance } from "compiler/Substance";
import im from "immutable";
import { A, C } from "types/ast";
import { Either } from "types/common";
import { Env } from "types/domain";
import { PenroseError } from "types/errors";
import { State } from "types/state";
import { StyProg } from "types/style";
import { Layer } from "types/styleSemantics";
import { SubProg, SubRes, SubstanceEnv } from "types/substance";
import { andThen, Result, showError, unsafelyUnwrap } from "utils/Error";
import { foldM, toLeft, ToRight } from "utils/Util";
import { compileDomain } from "./Domain";
import { envOrError, subEnvOrError } from "./Substance.test";

// TODO: Reorganize and name tests by compiler stage

// Load file in format "domain-dir/file.extension"
const loadFile = (examplePath: string): string => {
  // a bit hacky, only works with 2-part paths
  const [part0, part1] = examplePath.split("/");
  const prog = examples[part0][part1];
  return prog;
};

interface Trio {
  sub: string;
  dsl: string;
  sty: string;
}

const loadFiles = ({
  dslPath,
  subPath,
  styPath,
}: {
  dslPath: string;
  subPath: string;
  styPath: string;
}): Trio => ({
  dsl: loadFile(dslPath),
  sub: loadFile(subPath),
  sty: loadFile(styPath),
});

// Run the Domain + Substance parsers and checkers to yield the Style compiler's input
export const loadProgs = ({
  dsl,
  sub,
  sty,
}: Trio): {
  env: Env;
  subEnv: SubstanceEnv;
  subProg: SubProg<A>;
  styProg: StyProg<C>;
} => {
  const env: Env = envOrError(dsl);
  const [subEnv, varEnv]: [SubstanceEnv, Env] = subEnvOrError(sub, env);
  const styProg: StyProg<C> = unsafelyUnwrap(S.parseStyle(sty));
  return {
    env: varEnv,
    subEnv,
    subProg: subEnv.ast,
    styProg,
  };
};

const canvasPreamble = `canvas {
  width = 800
  height = 700
}
`;

describe("Layering computation", () => {
  // NOTE: again, for each edge (v, w), `v` is __below__ `w`.
  test("simple layering: A -> B -> C", () => {
    const partials: Layer[] = [
      { below: "A", above: "B" },
      { below: "B", above: "C" },
    ];
    const { shapeOrdering, warning } = S.computeShapeOrdering(
      ["A", "B", "C"],
      partials
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
    const { shapeOrdering, warning } = S.computeShapeOrdering(
      ["A", "B", "C"],
      partials
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
    const { shapeOrdering, warning } = S.computeShapeOrdering(
      ["A", "B", "C", "D", "E", "F"],
      partials
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
    const { shapeOrdering, warning } = S.computeShapeOrdering(
      ["A", "B", "C", "D", "E", "F"],
      partials
    );
    expect(shapeOrdering).toEqual(["A", "B", "D", "E", "C", "F"]);
    expect(warning).toBeDefined();
    expect(warning?.cycles.length).toEqual(1);
  });
});

describe("Compiler", () => {
  test("Label insertion", () => {
    const { env, subEnv, styProg } = loadProgs({
      dsl: "type Set",
      sub: `
      Set A, B, C
      AutoLabel All
      Label A $aaa$
      NoLabel B
      `,
      sty: ``,
    });
    const { substances } = buildAssignment(env, subEnv, styProg);
    for (const [name, label] of [
      ["A", "aaa"],
      ["B", ""],
      ["C", "C"],
    ]) {
      const v = substances.get(name)?.get("label");
      if (v?.tag === "OtherSource" && v?.expr.expr.tag === "StringLit") {
        expect(v.expr.expr.contents).toBe(label);
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

  test("substitution: S.uniqueKeysAndVals true", () => {
    // This subst has unique keys and vals
    expect(S.uniqueKeysAndVals({ a: "V", c: "z" })).toEqual(true);
  });

  test("substitution: S.uniqueKeysAndVals false", () => {
    // This subst doesn't have unique keys and vals
    expect(S.uniqueKeysAndVals({ a: "V", c: "V" })).toEqual(false);
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
  //     "linear-algebra-domain/linear-algebra.dsl",
  //     "linear-algebra-domain/twoVectorsPerp-unsugared.sub",
  //     "linear-algebra-domain/linear-algebra-paper-simple.sty",
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

  const sum = (acc: number, n: number, i: number): Either<string, number> =>
    i > 2 ? toLeft("error") : ToRight(acc + n);

  test("S.foldM none", () => {
    expect(foldM([], sum, -1)).toEqual(ToRight(-1));
  });

  test("S.foldM right", () => {
    expect(foldM([1, 2, 3], sum, -1)).toEqual(ToRight(5));
  });

  test("S.foldM left", () => {
    expect(foldM([1, 2, 3, 4], sum, -1)).toEqual(toLeft("error"));
  });

  const xs = ["a", "b", "c"];
  test("numbered", () => {
    expect(S.numbered(xs)).toEqual([
      ["a", 0],
      ["b", 1],
      ["c", 2],
    ]);
  });

  describe("Correct Style programs", () => {
    const domainProg = "type Object";
    const subProg = "Object o";
    // TODO: Name these programs
    const styProgs = [
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
        start: (0., ?)
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
      `canvas {
  width = 500.0
  height = 400.0
}`,
    ];

    const domainRes: Result<Env, PenroseError> = compileDomain(domainProg);

    const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
      (env) => compileSubstance(subProg, env),
      domainRes
    );

    for (const styProg of styProgs) {
      const fullProg = canvasPreamble + styProg;
      const styRes: Result<State, PenroseError> = andThen(
        (res) =>
          S.compileStyle(
            "Style compiler correctness test seed",
            fullProg,
            ...res
          ),
        subRes
      );

      if (!styRes.isOk()) {
        throw Error(
          `Expected Style program to work without errors:\n\n${fullProg}\nGot error: ${showError(
            styRes.error
          )}`
        );
      } else {
        expect(true).toEqual(true);
      }
    }
  });

  // TODO: There are no tests directly for the substitution application part of the compiler, though I guess you could walk the AST (making the substitution-application code more generic to do so) and check that there are no Style variables anywhere? Except for, I guess, namespace names?
  describe("Symmetric predicates", () => {
    test("non-symmetric predicate should not match", () => {
      const domainProg = `type Atom
      type Hydrogen <: Atom
      type Oxygen <: Atom
      predicate Bond(Atom, Atom)`;
      const subProg = `Hydrogen H
      Oxygen O
      Bond(H, O)`;
      const styProg =
        canvasPreamble +
        `forall Hydrogen h; Oxygen o
      where Bond(o, h) {
        myShape = Text {
          string: "Bond!"
        }
      }`;

      const domainRes: Result<Env, PenroseError> = compileDomain(domainProg);
      const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
        (env) => compileSubstance(subProg, env),
        domainRes
      );
      const styRes: Result<State, PenroseError> = andThen(
        (res) =>
          S.compileStyle(
            "Style compiler correctness test seed",
            styProg,
            ...res
          ),
        subRes
      );
      if (!styRes.isOk()) {
        throw Error(
          `Expected Style program to work without errors:\n\n${styRes}\nGot error: ${showError(
            styRes.error
          )}`
        );
      } else {
        expect(styRes.value.shapes.length).toEqual(0);
      }
    });
    test("symmetric predicate should match", () => {
      const domainProg = `type Atom
      type Hydrogen <: Atom
      type Oxygen <: Atom
      symmetric predicate Bond(Atom, Atom)`;
      const subProg = `Hydrogen H
      Oxygen O
      Bond(H, O)`;
      const styProg =
        canvasPreamble +
        `forall Hydrogen h; Oxygen o
      where Bond(o, h) {
        myShape = Text {
          string: "Bond!"
        }
      }`;

      const domainRes: Result<Env, PenroseError> = compileDomain(domainProg);
      const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
        (env) => compileSubstance(subProg, env),
        domainRes
      );
      const styRes: Result<State, PenroseError> = andThen(
        (res) =>
          S.compileStyle(
            "Style compiler correctness test seed",
            styProg,
            ...res
          ),
        subRes
      );
      if (!styRes.isOk()) {
        throw Error(
          `Expected Style program to work without errors:\n\n${styRes}\nGot error: ${showError(
            styRes.error
          )}`
        );
      } else {
        expect(styRes.value.shapes.length).toBeGreaterThan(0);
      }
    });
    test("nested symmetric predicates", () => {
      const domainProg = `type Atom
      type Hydrogen <: Atom
      type Oxygen <: Atom
      symmetric predicate Bond(Atom, Atom)
      predicate Not(Prop)`;
      const subProg = `Hydrogen H
      Oxygen O
      Not(Bond(H, O))`;
      const styProg =
        canvasPreamble +
        `forall Hydrogen h; Oxygen o
        where Not(Bond(o, h)) {
          theText = Text {
            string: "hello"
          }
        }`;
      const domainRes: Result<Env, PenroseError> = compileDomain(domainProg);
      const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
        (env) => compileSubstance(subProg, env),
        domainRes
      );
      const styRes: Result<State, PenroseError> = andThen(
        (res) =>
          S.compileStyle(
            "Style compiler correctness test seed",
            styProg,
            ...res
          ),
        subRes
      );
      if (!styRes.isOk()) {
        throw Error(
          `Expected Style program to work without errors:\n\n${styRes}\nGot error: ${showError(
            styRes.error
          )}`
        );
      } else {
        expect(styRes.value.shapes.length).toBeGreaterThan(0);
      }
    });
  });

  describe("number of matchings", () => {
    test("no double matching, non-symmetric", () => {
      const domainProg = `type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
predicate Bond(Atom, Atom)`;
      const subProg = `Hydrogen H1, H2
      Oxygen O
      Bond( O, H1 )
      Bond( O, H2 )`;
      const styProg =
        canvasPreamble +
        `forall Oxygen o; Hydrogen h1; Hydrogen h2
        where Bond(o,h1); Bond(o,h2) {
            myText = Text {
                string: "Water!"
                fillColor: rgba(0, 0, 0, 255)
            }
        }`;

      const domainRes: Result<Env, PenroseError> = compileDomain(domainProg);
      const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
        (env) => compileSubstance(subProg, env),
        domainRes
      );
      const styRes: Result<State, PenroseError> = andThen(
        (res) =>
          S.compileStyle(
            "Style compiler correctness test seed",
            styProg,
            ...res
          ),
        subRes
      );
      if (!styRes.isOk()) {
        throw Error(
          `Expected Style program to work without errors:\n\n${styRes}\nGot error: ${showError(
            styRes.error
          )}`
        );
      } else {
        expect(styRes.value.shapes.length).toEqual(1);
      }
    });

    test("no double matching, symmetric", () => {
      const domainProg = `type Atom
      symmetric predicate Bond(Atom, Atom)`;
      const subProg = `Atom A1, A2
      Bond( A1, A2 )`;
      const styProg =
        canvasPreamble +
        `forall Atom a1; Atom a2
        where Bond(a1, a2) {
            myText = Text {
                string: "Bond"
            }
        }`;
      const domainRes: Result<Env, PenroseError> = compileDomain(domainProg);
      const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
        (env) => compileSubstance(subProg, env),
        domainRes
      );
      const styRes: Result<State, PenroseError> = andThen(
        (res) =>
          S.compileStyle(
            "Style compiler correctness test seed",
            styProg,
            ...res
          ),
        subRes
      );
      if (!styRes.isOk()) {
        throw Error(
          `Expected Style program to work without errors:\n\n${styRes}\nGot error: ${showError(
            styRes.error
          )}`
        );
      } else {
        expect(styRes.value.shapes.length).toEqual(1);
      }
    });

    test("extra variables not in relations", () => {
      const domainProg = `type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
predicate Bond(Atom, Atom)`;
      const subProg = `Hydrogen H1, H2
      Oxygen O
      Bond( O, H1 )
      Bond( O, H2 )
      Hydrogen H3, H4`;
      const styProg =
        canvasPreamble +
        `forall Oxygen o; Hydrogen h1; Hydrogen h2; Hydrogen h3
        where Bond(o,h1); Bond(o,h2) {
            myText = Text {
                string: "Water!"
                fillColor: rgba(0, 0, 0, 255)
            }
        }`;

      const domainRes: Result<Env, PenroseError> = compileDomain(domainProg);
      const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
        (env) => compileSubstance(subProg, env),
        domainRes
      );
      const styRes: Result<State, PenroseError> = andThen(
        (res) =>
          S.compileStyle(
            "Style compiler correctness test seed",
            styProg,
            ...res
          ),
        subRes
      );
      if (!styRes.isOk()) {
        throw Error(
          `Expected Style program to work without errors:\n\n${styRes}\nGot error: ${showError(
            styRes.error
          )}`
        );
      } else {
        expect(styRes.value.shapes.length).toEqual(2);
      }
    });

    test("pure selector, no relations", () => {
      const domainProg = `type Atom`;
      const subProg = `Atom A1, A2`;
      const styProg =
        canvasPreamble +
        `forall Atom a1; Atom a2 {
            myText = Text {
                string: "TwoAtoms!"
                fillColor: rgba(0, 0, 0, 255)
            }
        }`;

      const domainRes: Result<Env, PenroseError> = compileDomain(domainProg);
      const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
        (env) => compileSubstance(subProg, env),
        domainRes
      );
      const styRes: Result<State, PenroseError> = andThen(
        (res) =>
          S.compileStyle(
            "Style compiler correctness test seed",
            styProg,
            ...res
          ),
        subRes
      );
      if (!styRes.isOk()) {
        throw Error(
          `Expected Style program to work without errors:\n\n${styRes}\nGot error: ${showError(
            styRes.error
          )}`
        );
      } else {
        expect(styRes.value.shapes.length).toEqual(1);
      }
    });
  });

  describe("predicate alias", () => {
    test("general predicate alias with symmetry", () => {
      const domainProg = `type Atom
type Hydrogen <: Atom
type Oxygen <: Atom
symmetric predicate Bond(Atom, Atom)
`;
      const substanceProg = `Hydrogen H1, H2
Oxygen O
Bond(O, H1)
Bond(O, H2)`;
      const styleProg =
        canvasPreamble +
        `
    forall Oxygen o; Hydrogen h
    where Bond(h, o) as b {
        b.shape = Line {
        }
    }
    `;
      const domainRes: Result<Env, PenroseError> = compileDomain(domainProg);
      const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
        (env) => compileSubstance(substanceProg, env),
        domainRes
      );
      const styRes: Result<State, PenroseError> = andThen(
        (res) =>
          S.compileStyle(
            "Style compiler correctness test seed",
            styleProg,
            ...res
          ),
        subRes
      );
      if (!styRes.isOk()) {
        throw Error(
          `Expected Style program to work without errors:\n\n${styRes}\nGot error: ${showError(
            styRes.error
          )}`
        );
      } else {
        expect(styRes.value.shapes.length).toEqual(2);
      }
    });
    test("correct style programs with predicate aliasing", () => {
      const domainProg = "type Set \n predicate IsSubset(Set, Set)";
      const subProg = "Set A\nSet B\nSet C\nIsSubset(B, A)\nIsSubset(C, B)";

      const styProg =
        canvasPreamble +
        `forall Set a; Set b where IsSubset(a,b) as foo {
          foo.icon = Rectangle{}
        }
        forall Set u; Set v where IsSubset(u,v) as bar {
          bar.icon2 = Ellipse{}
        }
        `;
      const domainRes: Result<Env, PenroseError> = compileDomain(domainProg);
      const subRes: Result<SubRes, PenroseError> = andThen(
        (env) => compileSubstance(subProg, env),
        domainRes
      );
      const styRes = andThen(
        (res) => S.compileStyle("", canvasPreamble + styProg, ...res),
        subRes
      );
      if (!styRes.isOk()) {
        throw new Error(
          `Expected Style program to work without errors. Got error ${styRes.error.errorType}`
        );
      }
      const state = styRes.value;
      expect(state.shapes.length).toEqual(4);
    });
  });
  // Test errors
  const PRINT_ERRORS = false;

  const expectErrorOf = (
    result: Result<State, PenroseError>,
    errorType: string
  ) => {
    if (result.isErr()) {
      const res: PenroseError = result.error;
      if (res.errorType !== "StyleError") {
        throw Error(
          `Error ${errorType} was supposed to occur. Got a non-Style error '${res.errorType}'.`
        );
      }

      if (res.tag !== "StyleErrorList") {
        throw Error(
          `Error ${errorType} was supposed to occur. Did not receive a Style list. Got ${res.tag}.`
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
    const subProg = loadFile("set-theory-domain/twosets-simple.sub");
    const domainProg = loadFile("set-theory-domain/functions.dsl");
    // We test variations on this Style program
    // const styPath = "set-theory-domain/venn.sty";

    const domainRes: Result<Env, PenroseError> = compileDomain(domainProg);

    const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
      (env) => compileSubstance(subProg, env),
      domainRes
    );

    const testStyProgForError = (styProg: string, errorType: string) => {
      let preamble = errorType.startsWith("Canvas") ? "" : canvasPreamble;
      const styRes: Result<State, PenroseError> = andThen(
        (res) =>
          S.compileStyle(
            "Style compiler errors test seed",
            preamble + styProg,
            ...res
          ),
        subRes
      );
      describe(errorType, () => {
        expectErrorOf(styRes, errorType);
      });
    };

    const errorStyProgs = {
      // ------ Selector errors (from Substance)
      SelectorVarMultipleDecl: [`forall Set x; Set x { }`],
      SelectorFieldNotSupported: [`forall Set x where x has randomfield { }`],

      // COMBAK: Style doesn't throw parse error if the program is just "forall Point `A`"... instead it fails inside compileStyle with an undefined selector environment
      SelectorDeclTypeMismatch: [`forall Point \`A\` { }`],

      SelectorRelTypeMismatch: [
        `forall Point x; Set y; Set z
      where x := Union(y, z) { } `,
      ],

      TaggedSubstanceError: [
        `forall Set x; Point y
where IsSubset(y, x) { }`,
        `forall Setfhjh x { }`,
        `forall Point x, y where Midpointdfsdfds(x, y) { }`,
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

      PropertyMemberError: [`forall Set x { delete y.z.p }`],
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
  width = ?
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
        where IsSubset(a, b) as a {}`,
        `forall Set a; Set b
        where IsSubset(a, b) as Set {}`,
        `forall Set a; Set b
        where IsSubset(a, b) as IsSubset {}`,
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

    // Test that each program yields its error type
    for (const [errorType, styProgs] of Object.entries(errorStyProgs)) {
      for (const styProg of styProgs) {
        // TODO(error): improve this so it becomes individual tests, using the framework
        // console.log("testing", errorType);
        testStyProgForError(styProg, errorType);
      }
    }
  });

  describe("faster matching", () => {
    test("multiple predicates", () => {
      const subProg = `
      MySet X, Y
 OtherType Z
 
 MyPred(Z, X, Y)
 MyOtherPred(X, Y)`;
      const domProg = `
     type MySet
 type OtherType
 
 predicate MyPred(OtherType, MySet, MySet)
 predicate MyOtherPred(MySet, MySet)`;

      const styProg =
        canvasPreamble +
        `
     forall MySet X; MySet Y; OtherType Z
 where MyPred(Z, X, Y); MyOtherPred(X, Y) {
     theCircle = Circle {
         r: 20
     }
 }`;
      const domainRes: Result<Env, PenroseError> = compileDomain(domProg);
      const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
        (env) => compileSubstance(subProg, env),
        domainRes
      );
      const styRes: Result<State, PenroseError> = andThen(
        (res) =>
          S.compileStyle(
            "Style compiler correctness test seed",
            styProg,
            ...res
          ),
        subRes
      );
      if (!styRes.isOk()) {
        throw Error(
          `Expected Style program to work without errors:\n\n${styRes}\nGot error: ${showError(
            styRes.error
          )}`
        );
      } else {
        expect(styRes.value.shapes.length).toEqual(1);
      }
    });
    test("many declaration matches with only one relational match", () => {
      const subProg = `
      T t1, t2, t3, t4, t5, t6, t7, t8
      S s := f( t1, t2, t3, t4, t5, t6, t7, t8 )`;
      const domProg = `
      -- minimal.dsl
      type S
      type T
      constructor f( T t1, T t2, T t3, T t4, T t5, T t6, T t7, T t8 ) -> S`;

      const styProg =
        canvasPreamble +
        `
        forall S s; T t1; T t2; T t3; T t4; T t5; T t6; T t7; T t8
        where s := f( t1, t2, t3, t4, t5, t6, t7, t8 ) {
           s.shape = Circle {
              center: (0,0)
              r: 10.0
           }
        }`;
      const domainRes: Result<Env, PenroseError> = compileDomain(domProg);
      const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
        (env) => compileSubstance(subProg, env),
        domainRes
      );
      const styRes: Result<State, PenroseError> = andThen(
        (res) =>
          S.compileStyle(
            "Style compiler correctness test seed",
            styProg,
            ...res
          ),
        subRes
      );
      if (!styRes.isOk()) {
        throw Error(
          `Expected Style program to work without errors:\n\n${styRes}\nGot error: ${showError(
            styRes.error
          )}`
        );
      } else {
        expect(styRes.value.shapes.length).toEqual(1);
      }
    });
  });

  describe("match metadata", () => {
    test("match total", () => {
      const domProg = "type MyType\n";
      const styProg =
        canvasPreamble +
        `forall MyType t {
  t.shape = Text {
    string: match_total
  }
}`;
      const subProg = "MyType t1, t2, t3\n";
      const domainRes: Result<Env, PenroseError> = compileDomain(domProg);
      const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
        (env) => compileSubstance(subProg, env),
        domainRes
      );
      const res: Result<State, PenroseError> = andThen(
        (res) =>
          S.compileStyle(
            "Style compiler correctness test seed",
            styProg,
            ...res
          ),
        subRes
      );
      if (!res.isOk()) {
        throw Error(
          `Expected Style program to work without errors:\n\n${res}\nGot error: ${showError(
            res.error
          )}`
        );
      } else {
        expect(
          res.value.shapes.every((shape) => {
            const val = shape.properties["string"];
            return val.tag === "FloatV" && val.contents === 3;
          })
        ).toEqual(true);
      }
    });

    test("match id", () => {
      const domProg = "type MyType\n";
      const styProg =
        canvasPreamble +
        `forall MyType t {
  t.shape = Text {
    string: match_id
  }
}`;
      const subProg = "MyType t1, t2, t3\n";
      const domainRes: Result<Env, PenroseError> = compileDomain(domProg);
      const subRes: Result<[SubstanceEnv, Env], PenroseError> = andThen(
        (env) => compileSubstance(subProg, env),
        domainRes
      );
      const res: Result<State, PenroseError> = andThen(
        (res) =>
          S.compileStyle(
            "Style compiler correctness test seed",
            styProg,
            ...res
          ),
        subRes
      );
      if (!res.isOk()) {
        throw Error(
          `Expected Style program to work without errors:\n\n${res}\nGot error: ${showError(
            res.error
          )}`
        );
      } else {
        // Require that the match_id's are exactly [1, 2, 3]
        expect(
          im.Set(
            res.value.shapes.map((shape) => {
              const val = shape.properties["string"];
              if (val.tag === "FloatV") {
                return val.contents;
              } else {
                throw Error("Should be a number");
              }
            })
          )
        ).toEqual(im.Set([1, 2, 3]));
      }
    });
  });
});
