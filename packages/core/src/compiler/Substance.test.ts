import nearley from "nearley";
import { beforeEach, describe, expect, test } from "vitest";
import grammar from "../parser/SubstanceParser.js";
import { A, Identifier } from "../types/ast.js";
import { DomainEnv } from "../types/domain.js";
import { PenroseError } from "../types/errors.js";
import { ApplyPredicate, SubstanceEnv } from "../types/substance.js";
import { Result, showError, showType } from "../utils/Error.js";
import { compileDomain } from "./Domain.js";
import {
  compileSubstance,
  prettySubstance,
  toLiteralName,
} from "./Substance.js";

const printError = false;

const hasVars = (subEnv: SubstanceEnv, vars: [string, string][]) => {
  vars.forEach(([name, type]: [string, string]) => {
    expect(subEnv.objs.has(name)).toBe(true);
    expect(showType(subEnv.objs.get(name)!)).toEqual(type);
  });
};

const domainProg = `
type Set
type OpenSet
type Vector
type Point
OpenSet <: Set
constructor Subset(Set A, Set B) -> Set
constructor Intersection(Set A, Set B) -> Set
function AddToOpen(Point p, OpenSet s) -> OpenSet
function AddPoint(Point p, Set s1) -> Set
predicate Empty(Set s)
predicate Intersecting(Set s1, Set s2)
predicate IsSubset(Set s1, Set s2)
`;

export const envOrError = (prog: string): DomainEnv => {
  const res = compileDomain(prog);
  if (res.isErr()) throw Error(showError(res.error));
  return res.value;
};

export const subEnvOrError = (
  prog: string,
  domEnv: DomainEnv,
): SubstanceEnv => {
  const res = compileSubstance(prog, domEnv);
  if (res.isOk()) {
    return res.value;
  } else {
    throw Error(`unexpected error ${showError(res.error)}`);
  }
};

let parser: nearley.Parser;
beforeEach(() => {
  // NOTE: Neither `feed` nor `finish` will reset the parser state. Therefore recompiling before each unit test
  parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));
});

describe("Common", () => {
  test("empty program", () => {
    const prog = ``;
    const env = envOrError(domainProg);
    const res = compileSubstance(prog, env);
    expect(res.isOk()).toBe(true);
  });
  test("trailing comment", () => {
    const prog = `
Set A
Set B
Set C
Set D
-- Set E`;
    const env = envOrError(domainProg);
    const res = compileSubstance(prog, env);
    expect(res.isOk()).toBe(true);
  });
});

describe("Postprocess", () => {
  test("labels 1", () => {
    const prog = `
Set A, B, C, D, E
AutoLabel All
Label A $\\vec{A}$
Label B "B_1"
NoLabel D, E
    `;
    const env = envOrError(domainProg);
    const res = compileSubstance(prog, env);
    if (res.isOk()) {
      const expected = [
        ["A", "\\vec{A}", "MathLabel"],
        ["B", "B_1", "TextLabel"],
        ["C", "C", "MathLabel"],
        ["D", "", "NoLabel"],
        ["E", "", "NoLabel"],
      ];
      const labelMap = res.value.labels;
      expected.forEach(([id, value, type]) => {
        const label = labelMap.get(id)!;
        expect(label.value).toEqual(value);
        expect(label.type).toEqual(type);
      });
    } else {
      throw Error(
        "Unexpected error when processing labels: " + showError(res.error),
      );
    }
  });

  test("labels 2: literals", () => {
    const dom = `
      predicate P(Number)
      predicate Q(String)`;
    const domEnv = envOrError(dom);
    const sub = `
      P(1)
      P(-2.5)
      Q("Hello")
      Q("#123")
      AutoLabel All`;
    const { labels } = subEnvOrError(sub, domEnv);

    [1, -2.5].forEach((n) => {
      const name = toLiteralName(n);
      expect(labels.get(name)).toBeDefined();
      expect(labels.get(name)!.value).toEqual(n.toString());
    });

    ["Hello", "#123"].forEach((s) => {
      const name = toLiteralName(s);
      expect(labels.get(name)).toBeDefined();
      expect(labels.get(name)!.value).toEqual(s);
    });
  });
});

describe("Check statements", () => {
  test("decls", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B, C
OpenSet D
A := D
    `;
    const res = compileSubstance(prog, env);
    expect(res.isOk()).toBe(true);
    if (res.isOk()) {
      hasVars(res.value, [["A", "Set"]]);
    }
  });
  test("decl bind", () => {
    const env = envOrError(domainProg);
    const prog = `
OpenSet D
Set B, C
Set A := D
Set E := Subset(B, C)
    `;
    const res = compileSubstance(prog, env);
    expect(res.isOk()).toBe(true);
    if (res.isOk()) {
      // TODO: not caching var bindings for now. Add to checker if needed
      // expect(res.value[1].bindings.get("A")![0].name.value).toEqual("Subset");
      hasVars(res.value, [
        ["A", "Set"],
        ["E", "Set"],
        ["D", "OpenSet"],
      ]);
    }
  });
  test("func: function", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B
Point p
B := AddPoint(p, B)
      `;
    const res = compileSubstance(prog, env);
    if (res.isOk()) {
      hasVars(res.value, [
        ["A", "Set"],
        ["B", "Set"],
        ["p", "Point"],
      ]);
    } else {
      throw Error(`unexpected error ${showError(res.error)}`);
    }
  });
  test("predicates: non-nesting", () => {
    const prog = `
Set A, B, C, D, E
C := Intersection(A, B)
Empty(C)
IsSubset(D, E)
IsSubset(D, A)
    `;
    const env = envOrError(domainProg);
    subEnvOrError(prog, env);
  });
  test("labeling", () => {
    const prog = `
Set A, B, C
Label A $\\vec{A}$
Label B $B_1$
AutoLabel All
AutoLabel B, C
NoLabel B, C
    `;
    const env = envOrError(domainProg);
    subEnvOrError(prog, env);
  });
  describe("indexed expressions", () => {
    test("indexed set decl - sets with even indices", () => {
      const env = envOrError(domainProg);
      const prog = `Set a_i for i in [1, 10] where i % 2 == 0`;
      const res = compileSubstance(prog, env);
      expect(res.isOk()).toBe(true);
      if (res.isOk()) {
        hasVars(res.value, [
          ["a_2", "Set"],
          ["a_4", "Set"],
          ["a_6", "Set"],
          ["a_8", "Set"],
          ["a_10", "Set"],
        ]);
      }
    });

    test("indexed set decl - no satisfying indices", () => {
      const env1 = envOrError(domainProg);
      const prog1 = `Set a_i for i in [1, 10] where i % 2 == 0.5`;
      const res1 = compileSubstance(prog1, env1);
      expect(res1.isOk()).toBe(true);
      if (res1.isOk()) {
        expect(res1.value.objs.size).toBe(0);
      }

      const env2 = envOrError(domainProg);
      const prog2 = `Set a_i for i in [20, 10]`;
      const res2 = compileSubstance(prog2, env2);
      expect(res2.isOk()).toBe(true);
      if (res2.isOk()) {
        expect(res2.value.objs.size).toBe(0);
      }

      const env3 = envOrError(domainProg);
      const prog3 = `Set a_i, b_j for i in [20, 1], j in [1, 20]`;
      const res3 = compileSubstance(prog3, env3);
      expect(res3.isOk()).toBe(true);
      if (res3.isOk()) {
        expect(res3.value[1].vars.size).toBe(0);
      }
    });

    test("indexed set decllist", () => {
      const env = envOrError(domainProg);
      const prog = `Set a_i, a_j for i in [0, 9], j in [0, 9] where i % 2 == 0 && j == i + 1`;
      // first iteration: a_0, a_1
      // second iteration: a_2, a_3
      // third iteration: a_4, a_5
      // ...
      const res = compileSubstance(prog, env);
      expect(res.isOk()).toBe(true);
      if (res.isOk()) {
        hasVars(
          res.value,
          [0, 1, 2, 3, 4, 5, 6, 7, 8, 9].map((n) => [`a_${n}`, "Set"]),
        );
      }
    });

    test("indexed set pred", () => {
      const env = envOrError(domainProg);
      const prog = `
        Set s_i for i in [0, 5]
        IsSubset(s_i, s_j) for i in [0, 4], j in [1, 5] where j == i + 1
      `;
      const res = compileSubstance(prog, env);
      expect(res.isOk()).toBe(true);
      if (res.isOk()) {
        const preds = res.value.ast.statements.filter(
          (s) => s.tag === "ApplyPredicate",
        ) as ApplyPredicate<A>[];
        expect(preds.length).toBe(5);
        expect(preds.every((p) => p.name.value === "IsSubset")).toBe(true);
        expect(
          preds
            .map((p) =>
              [
                (p.args[0] as Identifier<A>).value,
                (p.args[1] as Identifier<A>).value,
              ].join(" "),
            )
            .sort(),
        ).toEqual(
          ["s_0 s_1", "s_1 s_2", "s_2 s_3", "s_3 s_4", "s_4 s_5"].sort(),
        );
      }
    });
  });
  test("numbers and strings", () => {
    const domain = `
    type Set
    predicate ContainsNum(Set, Number)
    predicate ContainsStr(Set, String)
    `;
    const prog = `
    Set A, B
    ContainsNum(A, 100)
    ContainsStr(B, "BBBB")
    ContainsNum(B, 100)
        `;
    const env = envOrError(domain);
    const subEnv = subEnvOrError(prog, env);
    ["A", "B", "{n100}", "{sBBBB}"].forEach((name) =>
      expect(subEnv.objs.get(name)).toBeDefined(),
    );

    // Even though literal 100 is used twice, we only want to have it recreated once.
    expect(subEnv.objs.count((_, name) => name === "{n100}")).toEqual(1);
  });
});

describe("Errors", () => {
  const expectErrorOf = (
    result: Result<SubstanceEnv, PenroseError>,
    errorType: PenroseError["tag"],
  ) => {
    if (result.isErr()) {
      if (printError) console.log(showError(result.error));
      expect(result.error.tag).toBe(errorType);
    } else {
      throw Error(`Error ${errorType} was suppoed to occur.`);
    }
  };
  test("parse error", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B, C ;;; shouldn't parse
    `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "ParseError");
  });
  test("duplicate name error", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A
Point A
AutoLabel All
    `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "DuplicateName");

    // some indexed expressions
    const env1 = envOrError(domainProg);
    const prog1 = `Point p_i, p_j for i in [1, 2], j in [1, 2]`;
    const res1 = compileSubstance(prog1, env1);
    expectErrorOf(res1, "DuplicateName");

    const env2 = envOrError(domainProg);
    const prog2 = `Point p_10
    Point p_j for j in [1, 11]`;
    const res2 = compileSubstance(prog2, env2);
    expectErrorOf(res2, "DuplicateName");

    const env3 = envOrError(domainProg);
    const prog3 = `Point p_i, p_i for i in [1, 10]`;
    const res3 = compileSubstance(prog3, env3);
    expectErrorOf(res3, "DuplicateName");
  });
  test("decl type not found", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B, C
Alien a
NotExistentType b
    `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "TypeNotFound");

    // test set
    const env1 = envOrError(domainProg);
    const prog1 = `Sset a_i, a_j for i in [10, 20], j in [20, 30]`;
    const res1 = compileSubstance(prog1, env1);
    expectErrorOf(res1, "TypeNotFound");
  });
  test("func type not found", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B, C
C := NotExistentFunc(A, B)
    `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "TypeNotFound");
  });
  test("var not found", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B, C
D := Subset(B, C)
    `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "VarNotFound");
  });
  test("type mismatch: var", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B
Vector v
A := B -- ok
A := v -- error
    `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "TypeMismatch");
  });
  test("func: arg length mismatch", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B, C
C := Subset(A) -- error
    `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "ArgLengthMismatch");
  });
  test("func: arg type mismatch", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B, C
Vector v
C := Subset(A, B) -- ok
C := Subset(A, v) -- error
    `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "TypeMismatch");
  });
  test("func: output type mismatch", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B, C
Vector v
C := Subset(A, B) -- ok
v := Subset(A, B) -- error
    `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "TypeMismatch");
  });
  test("func: argument of too general type", () => {
    const env = envOrError(domainProg);
    const prog = `
Set s
Point p
Set b := AddToOpen(p, s)
        `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "TypeMismatch");
  });
  test("wrong return type of anon constructor in predicate", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B, C
C := Subset(A, B)
Set D
D := AddPoint(C, B)
        `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "TypeMismatch");
  });
  test("variables not found in label statements", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B
Label D $\\vec{d}$
    `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "VarNotFound");
  });

  test("invalid index", () => {
    const env1 = envOrError(domainProg);
    const prog1 = `Set a_i for j in [1, 10]`;
    const res1 = compileSubstance(prog1, env1);
    expectErrorOf(res1, "InvalidSetIndexingError");

    const env2 = envOrError(domainProg);
    const prog2 = `Set a_i for i in [1, 10] where hello > i`;
    const res2 = compileSubstance(prog2, env2);
    expectErrorOf(res2, "InvalidSetIndexingError");
  });

  test("invalid range", () => {
    const env1 = envOrError(domainProg);
    const prog1 = `Set a_i for i in [1.234, 10]`;
    const res1 = compileSubstance(prog1, env1);
    expectErrorOf(res1, "BadSetIndexRangeError");
  });

  test("duplicate index", () => {
    const env = envOrError(domainProg);
    const prog = `Set a_i for i in [1, 10], j in [2, 10], i in [1, 10]`;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "DuplicateIndexError");
  });

  test("divide by zero in index condition", () => {
    const env = envOrError(domainProg);
    const prog = `Set a_i for i in [1, 10] where i / (i - i) == 3`;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "DivideByZeroError");
  });

  test("NaN in index condition arithmetics", () => {
    const env = envOrError(domainProg);
    const prog = `Set a_i for i in [1, 10] where (-i) ^ 0.5 == 1 `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "InvalidArithmeticValueError");
    // error because -1 ^ 0.5 is NaN
  });

  test("wrong use of builtin literal types", () => {
    const dom = `type Set`;
    const domEnv = envOrError(dom);
    const sub = `Number n := 100
String s`;
    const res = compileSubstance(sub, domEnv);
    expectErrorOf(res, "DeclLiteralError");
  });
});

describe("Subtypes", () => {
  test("func argument subtypes", () => {
    const env = envOrError(domainProg);
    const prog = `
    Set S
    OpenSet O
    Set B
    B := Subset(S, O)
        `;
    subEnvOrError(prog, env);
  });
});

describe("Pretty printer", () => {
  // Don't test for aggregated or indexed statements
  // since they get expanded during compilation.
  test("decls and args", () => {
    const env = envOrError(domainProg);
    const prog = `Set A
Set B
Set C
Set D
Set E
Vector v
C := Subset(A, B)
OpenSet Z
Empty(C)
IsSubset(D, E)
IsSubset(D, A)
AutoLabel All
Label A $\\vec{A}$
Label B $B_1$
NoLabel D, E`;
    const res = compileSubstance(prog, env);
    sameAsSource(prog, res);
  });
});

const sameAsSource = (
  source: string,
  res: Result<SubstanceEnv, PenroseError>,
) => {
  if (res.isOk()) {
    const ast = res.value.ast;
    const strFromAST = prettySubstance(ast);
    expect(strFromAST).toEqual(source);
  } else {
    throw Error(`unexpected error ${showError(res.error)}`);
  }
};
