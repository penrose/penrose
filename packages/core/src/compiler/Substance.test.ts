import nearley from "nearley";
import { beforeEach, describe, expect, test } from "vitest";
import grammar from "../parser/SubstanceParser.js";
import { A, Identifier } from "../types/ast.js";
import { Env } from "../types/domain.js";
import { PenroseError } from "../types/errors.js";
import { ApplyPredicate, SubRes, SubstanceEnv } from "../types/substance.js";
import { Result, showError, showType } from "../utils/Error.js";
import { compileDomain } from "./Domain.js";
import { compileSubstance, prettySubstance } from "./Substance.js";

const printError = false;

const hasVars = (env: Env, vars: [string, string][]) => {
  vars.forEach(([name, type]: [string, string]) => {
    expect(env.vars.has(name)).toBe(true);
    expect(showType(env.vars.get(name)!)).toEqual(type);
  });
};

const domainProg = `
type Set
type OpenSet
type Vector
type List('T)
type Tuple('T, 'U)
type Point
OpenSet <: Set
constructor Subset(Set A, Set B) -> Set
constructor Intersection(Set A, Set B) -> Set
constructor Cons ['X] ('X head, List('X) tail) -> List('X)
constructor Nil['X]() -> List('X)
constructor CreateTuple['T, 'U]('T fst, 'U snd) -> Tuple('T, 'U)
function AddPoint(Point p, Set s1) -> Set
predicate Not(Prop p1)
predicate Both(Prop p1, Prop p2)
predicate Empty(Set s)
predicate Intersecting(Set s1, Set s2)
predicate IsSubset(Set s1, Set s2)
`;

const domainProgWithPrelude = `
type Set
type OpenSet
type Vector
type List('T)
type Tuple('T, 'U)
type Point
OpenSet <: Set
constructor Subset(Set A, Set B) -> Set
constructor Intersection(Set A, Set B) -> Set
constructor Cons ['X] ('X head, List('X) tail) -> List('X)
constructor Nil['X]() -> List('X)
constructor CreateTuple['T, 'U]('T fst, 'U snd) -> Tuple('T, 'U)
function AddPoint(Point p, Set s1) -> Set
predicate Not(Prop p1)
predicate Both(Prop p1, Prop p2)
predicate Empty(Set s)
predicate Intersecting(Set s1, Set s2)
predicate IsSubset(Set s1, Set s2)
value X: Set
`;

export const envOrError = (prog: string): Env => {
  const res = compileDomain(prog);
  if (res.isErr()) throw Error(showError(res.error));
  return res.value;
};

export const subEnvOrError = (prog: string, env: Env): SubRes => {
  const res = compileSubstance(prog, env);
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
  test("preludes", () => {
    const env = envOrError(domainProgWithPrelude);
    const prog = `
Set A, B, C
List(Set) l
OpenSet D
A := D
    `;
    const res = compileSubstance(prog, env);
    expect(res.isOk()).toBe(true);
    if (res.isOk()) {
      hasVars(res.value[1], [
        ["A", "Set"],
        ["X", "Set"], // defined in prelude
        ["l", "List(Set)"],
      ]);
    }
  });
});

describe("Postprocess", () => {
  test("labels", () => {
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
      const labelMap = res.value[0].labels;
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
});

describe("Check statements", () => {
  test("decls", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B, C
List(Set) l
OpenSet D
A := D
    `;
    const res = compileSubstance(prog, env);
    expect(res.isOk()).toBe(true);
    if (res.isOk()) {
      hasVars(res.value[1], [
        ["A", "Set"],
        ["l", "List(Set)"],
      ]);
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
      expect(res.value[1].constructorsBindings.get("E")![0].name.value).toEqual(
        "Subset",
      );
      // TODO: not caching var bindings for now. Add to checker if needed
      // expect(res.value[1].bindings.get("A")![0].name.value).toEqual("Subset");
      hasVars(res.value[1], [
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
      hasVars(res.value[1], [
        ["A", "Set"],
        ["B", "Set"],
        ["p", "Point"],
      ]);
    } else {
      throw Error(`unexpected error ${showError(res.error)}`);
    }
  });
  test("func: constructor", () => {
    const env = envOrError(domainProg);
    const prog = `
List(Set) l, nil
nil := Nil()
Set A
l := Cons(A, nil)
      `;
    const res = compileSubstance(prog, env);
    if (res.isOk()) {
      hasVars(res.value[1], [
        ["A", "Set"],
        ["l", "List(Set)"],
        ["nil", "List(Set)"],
      ]);
    } else {
      throw Error(`unexpected error ${showError(res.error)}`);
    }
  });
  test("deconstructor: plain types", () => {
    const prog = `
Set A, B, C, D, E
C := Subset(A, B)
D := C.A
E := C.B
    `;
    const env = envOrError(domainProg);
    subEnvOrError(prog, env);
  });
  test("predicates: non-nesting", () => {
    const prog = `
Set A, B, C, D, E
C := Intersection(A, B)
Empty(C)
IsSubset(D, E)
IsSubset(D, A)
IsSubset(Subset(D, E), A) -- anon. constructor
    `;
    const env = envOrError(domainProg);
    subEnvOrError(prog, env);
  });
  test("predicates: nesting", () => {
    const prog = `
Set A, B, C, D, E
C := Intersection(A, B)
Not(Empty(C))
Not(IsSubset(D, E))
Not(IsSubset(Subset(D, E), A)) -- anon. constructor
Both(IsSubset(A, B), IsSubset(C, D))
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
        hasVars(res.value[1], [
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
        expect(res1.value[1].vars.size).toBe(0);
      }

      const env2 = envOrError(domainProg);
      const prog2 = `Set a_i for i in [11, 10]`;
      const res2 = compileSubstance(prog2, env2);
      expect(res2.isOk()).toBe(true);
      if (res2.isOk()) {
        expect(res2.value[1].vars.size).toBe(0);
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
          res.value[1],
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
        const preds = res.value[0].ast.statements.filter(
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
});

describe("Errors", () => {
  const expectErrorOf = (
    result: Result<[SubstanceEnv, Env], PenroseError>,
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
List(Set) l
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
List(Set) l
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
  // TODO: fix typeconstructor check and pass this test
  test("func: type argument mismatch", () => {
    const env = envOrError(domainProg);
    const prog = `
-- type Tuple('T, 'U)
-- constructor CreateTuple['T, 'U]('T fst, 'U snd) -> Tuple('T, 'U)
List(Set) nil
Tuple(Set, Set) t -- Maybe an error?
t := CreateTuple(nil, nil) -- Definitely an error
      `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "TypeMismatch");
  });
  test("func: type argument mismatch 2", () => {
    const env = envOrError(domainProg);
    const prog = `
  -- Substance program for type checking
  List(Set) l, nil
  nil := Nil()
  Set A
  l := Cons(nil, A)
      `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "TypeMismatch");
  });
  test("func: argument of too general type", () => {
    const env = envOrError(domainProg);
    const prog = `
List(OpenSet) l, nil
nil := Nil()
Set A
l := Cons(A, nil)
        `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "TypeMismatch");
  });
  test("unbound field access", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B
B := A.field
        `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "DeconstructNonconstructor");
  });
  test("unbound field access of a function", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B,
Point p, q
B := AddPoint(p, A)
q := B.p1 -- although the function has named args, one still cannot deconstruct functions. Only constructors are okay.
        `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "DeconstructNonconstructor");
  });
  test("wrong return type of anon constructor in predicate", () => {
    const env = envOrError(domainProg);
    const prog = `
List(Set) nil
nil := Nil()
OpenSet A
IsSubset(nil, A) -- error because nil is not a Set
        `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "TypeMismatch");
  });
  test("unexpected var when nested pred is expected", () => {
    const env = envOrError(domainProg);
    const prog = `
Set A, B
Not(IsSubset(A, B)) -- ok
Not(Intersection(A, B))
    `;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "UnexpectedExprForNestedPred");
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

  test("unsupported indexing statement", () => {
    const env = envOrError(domainProg);
    const prog = `Set a, b
    IsSubset(a, b) <-> IsSubset(b, a) for i in [1, 10]`;
    const res = compileSubstance(prog, env);
    expectErrorOf(res, "UnsupportedIndexingError");
  });
});

describe("Subtypes", () => {
  test("func argument subtypes", () => {
    const env = envOrError(domainProg);
    const prog = `
List(Set) l, nil
nil := Nil()
OpenSet A
l := Cons(A, nil)
        `;
    subEnvOrError(prog, env);
  });
  test("func argument parametrized subtypes", () => {
    const env = envOrError(domainProg);
    const prog = `
List(Set) l
List(OpenSet) nil
nil := Nil()
OpenSet A
l := Cons(A, nil)
        `;
    subEnvOrError(prog, env);
  });
});
describe("Ambiguous expressions", () => {
  test("nested predicates and functions parsed as Func", () => {
    const env = envOrError(domainProg);
    const prog = `Set A, B
Point p
Not(Intersecting(A, B))
Empty(Subset(A, B))
Empty(AddPoint(p, A))`;
    const [subEnv] = subEnvOrError(prog, env);
    expect((subEnv.ast.statements[3] as ApplyPredicate<A>).args[0].tag).toEqual(
      "ApplyPredicate",
    );
    expect((subEnv.ast.statements[4] as ApplyPredicate<A>).args[0].tag).toEqual(
      "ApplyConstructor",
    );
    expect((subEnv.ast.statements[5] as ApplyPredicate<A>).args[0].tag).toEqual(
      "ApplyFunction",
    );
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
D := C.A
E := C.B
List(Set) l
List(OpenSet) nil
OpenSet Z
nil := Nil()
l := Cons(Z, nil)
Empty(C)
IsSubset(D, E)
IsSubset(D, A)
Not(IsSubset(D, A))
Not(Intersecting(B, C))
IsSubset(A, B) <-> IsSubset(B, C)
Subset(A, B) = Subset(B, C)
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
  res: Result<[SubstanceEnv, Env], PenroseError>,
) => {
  if (res.isOk()) {
    const ast = res.value[0].ast;
    const strFromAST = prettySubstance(ast);
    expect(strFromAST).toEqual(source);
  } else {
    throw Error(`unexpected error ${showError(res.error)}`);
  }
};
