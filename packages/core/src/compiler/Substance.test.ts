import { examples } from "@penrose/examples";
import * as fs from "fs";
import * as nearley from "nearley";
import grammar from "parser/SubstanceParser";
import * as path from "path";
import { Env } from "types/domain";
import { PenroseError } from "types/errors";
import { SubstanceEnv } from "types/substance";
import { Result, showError, showType } from "utils/Error";
import { compileDomain } from "./Domain";
import { compileSubstance, prettySubstance } from "./Substance";

const printError = false;
const saveContexts = false;
const outputDir = "/tmp/contexts";

const subPaths = [
  // "linear-algebra-domain/twoVectorsPerp.sub",
  ["set-theory-domain/setTheory.dsl", "set-theory-domain/tree.sub"],
  ["set-theory-domain/setTheory.dsl", "set-theory-domain/continuousmap.sub"],
  ["set-theory-domain/setTheory.dsl", "set-theory-domain/twosets-simple.sub"],
  ["set-theory-domain/setTheory.dsl", "set-theory-domain/multisets.sub"],
  ["set-theory-domain/setTheory.dsl", "set-theory-domain/nested.sub"],
  // "hyperbolic-domain/hyperbolic-example.sub",
  // "geometry-domain/pythagorean-theorem-sugared.sub",
  // "mesh-set-domain/DomainInterop.sub",
];

const hasVars = (env: Env, vars: [string, string][]) => {
  vars.map(([name, type]: [string, string]) => {
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

const envOrError = (prog: string): Env => {
  const res = compileDomain(prog);
  if (res.isErr()) fail(showError(res.error));
  return res.value;
};

const compileOrError = (prog: string, env: Env) => {
  const res = compileSubstance(prog, env);
  if (res.isOk()) {
    return;
  } else {
    fail(`unexpected error ${showError(res.error)}`);
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
      expected.map(([id, value, type]) => {
        const label = labelMap.get(id)!;
        expect(label.value).toEqual(value);
        expect(label.type).toEqual(type);
      });
    } else {
      fail("Unexpected error when processing labels: " + showError(res.error));
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
        "Subset"
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
      fail(`unexpected error ${showError(res.error)}`);
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
      fail(`unexpected error ${showError(res.error)}`);
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
    compileOrError(prog, env);
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
    compileOrError(prog, env);
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
    compileOrError(prog, env);
  });
  test("predicates: nesting", () => {
    const prog = `
Set A, B, C
Label A $\\vec{A}$
Label B $B_1$
AutoLabel All
AutoLabel B, C
NoLabel B, C
    `;
    const env = envOrError(domainProg);
    compileOrError(prog, env);
  });
});

describe("Errors", () => {
  const expectErrorOf = (
    result: Result<[SubstanceEnv, Env], PenroseError>,
    errorType: string
  ) => {
    if (result.isErr()) {
      if (printError) console.log(showError(result.error));
      expect(result.error.tag).toBe(errorType);
    } else {
      fail(`Error ${errorType} was suppoed to occur.`);
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
    compileOrError(prog, env);
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
    compileOrError(prog, env);
  });
});

describe("Real Programs", () => {
  // create output folder
  if (saveContexts && !fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir);
  }

  subPaths.map(([domainPath, examplePath]) => {
    // a bit hacky, only works with 2-part paths
    const [domPart0, domPart1] = domainPath.split("/");
    const [subPart0, subPart1] = examplePath.split("/");
    const domProg = examples[domPart0][domPart1];
    const subProg = examples[subPart0][subPart1];
    test(examplePath, () => {
      // do testing
      const env = envOrError(domProg);
      const res = compileSubstance(subProg, env);
      expect(res.isOk()).toBe(true);
      // write to output folder
      if (res.isOk() && saveContexts) {
        const domainName = path.basename(domainPath, ".dsl");
        const exampleName = path.basename(examplePath, ".sub");
        const envPath = path.join(outputDir, domainName + ".env.json");
        const subenvPath = path.join(outputDir, exampleName + ".env.json");
        fs.writeFileSync(subenvPath, JSON.stringify(res.value[0]), "utf8");
        fs.writeFileSync(envPath, JSON.stringify(res.value[1]), "utf8");
      }
    });
  });
});

describe("Pretty printer", () => {
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
  res: Result<[SubstanceEnv, Env], PenroseError>
) => {
  if (res.isOk()) {
    const ast = res.value[0].ast;
    const strFromAST = prettySubstance(ast);
    expect(strFromAST).toEqual(source);
  } else {
    fail(`unexpected error ${showError(res.error)}`);
  }
};
