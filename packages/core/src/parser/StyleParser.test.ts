import nearley from "nearley";
import { beforeEach, describe, expect, test } from "vitest";
import { parseStyle } from "../compiler/Style.js";
import { C } from "../types/ast.js";
import { StyProg } from "../types/style.js";
import grammar from "./StyleParser.js";

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
  test("unbalanced curly", () => {
    const prog = `
    forall Set x {
      x.shape = Circle {
  }
    `;
    expect(parseStyle(prog).isErr()).toEqual(true);
  });
  test("type keyword check", () => {
    const prog = `
const {
  A.shape = Circle {}
  B.icon.x = 2.5
}
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
    const ast = results[0];
    const keyword = ast.items[0].block.statements[0].path.members[0];
    const id = ast.items[0].block.statements[1].path.members[0];

    expect(keyword.type).toEqual("type-keyword");
    expect(id.type).toEqual("identifier");
  });
});

describe("Selector Grammar", () => {
  test("empty namespace block", () => {
    const { results } = parser.feed("const { }");
    sameASTs(results);
  });

  test("empty selector block", () => {
    const { results } = parser.feed("forall Set A{}");
    sameASTs(results);
    const ast: StyProg<C> = results[0] as StyProg<C>;
  });

  test("comment and empty blocks", () => {
    const prog = `
  -- this is a comment

  forall Set A with Set B {

  }

  -- commented out block
  -- -- -- -- forall Set A with Set B -- {

  -- -- -- -- }

  forall Set A, \`B\`; Map f
  {
  }`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("forall keyword", () => {
    const prog = `
  forall Set B { }

  forall Set A, B { }

  forall Set \`C\` { }

  forall Set A, B { }

  forall repeatable Set A, \`B\`; Map f
  {

  }`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("nested preds", () => {
    const prog = `
forall Set A, B, C
where IsSubset(Union(A,B), C, Intersection(B, C));
{ }`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("empty pred", () => {
    const prog = `
forall Set A, B, C
where IsSubset(   );
{ }`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("with, where, and as", () => {
    const prog = `
forall Set A, B; Map f
with Set C, D; Map \`g\`
where C := intersect ( A, B, Not(f) ) ; IsSubset( A, B ); IsSubset( Union(A,B), C); Intersect (   )
as Const
{ }`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("collector", () => {
    const p1 = `collect Element e into es {}`;
    const r1 = parser.feed(p1);
    sameASTs(r1.results);

    const p2 = `collect Element e
    into es where In(e, s) with Set s {}`;
    const r2 = parser.feed(p2);
    sameASTs(r2.results);

    const p3 = `collect Element e
    into es where In(e, s) foreach Set s {}`;
    const r3 = parser.feed(p3);
    sameASTs(r3.results);

    const p4 = `collect Element e
    into es where In(e, s) with Set s
    foreach OtherThing o {}`;
    const r4 = parser.feed(p4);
    sameASTs(r4.results);

    const p5 = `collect Element e
    into es foreach OtherThing o where In(e, s) 
    with Set s {}`;
    const r5 = parser.feed(p5);
    sameASTs(r5.results);

    const p6 = `collect Element e
    into es 
    with Set s foreach OtherThing o where In(e, s) {} `;
    const r6 = parser.feed(p6);
    sameASTs(r6.results);
  });
  test("multiple as clauses for predicates", () => {
    const prog = `
forall Set A, B, C, D
where IsSubset(A, B) as foo; IsSubset(B,C) as bar; Union(C,D) as yeet;
{
  foo.arrow = Arrow{}
  yeet.circle = Circle{}
  bar.square = Square{}
}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
  test("alias general test", () => {
    const prog =
      "\
  forall Atom a1; Atom a2 \
  where Bond(a1, a2) as b {}";
    const { results } = parser.feed(prog);
    expect(results[0].items[0].header.where.contents[0].alias.value).toEqual(
      "b",
    );
  });
  test("alias expression location test", () => {
    // why is this failing bro
    const prog =
      "\
forall Set A, B, C; Map f \
with Set C, D; Map `g` \
where C := intersect ( A, B, Not(f) ) ; IsSubset( A, B ) as isSubsetAB; IsSubset( Union(A,B), C) as nested_Subset; Intersect (   ){}\
";
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("alias expression location test, weird spacing", () => {
    // why is this failing bro
    const prog =
      "\
forall Set A, B, C; Map f \
with Set C, D; Map `g` \
where C := intersect ( A, B, Not(f) ) ;\
 IsSubset( A, B ) \
 as isSubsetAB; IsSubset( Union(A,B), C) as nested_Subset; \
 Intersect (   )  as   ok {}\
";
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("cannot set as clauses for decls", () => {
    const prog = `forall Set A, B; Map f as Const -- should fail because decls can't be aliased
`;
    expect(parseStyle(prog).isErr()).toEqual(true);
  });
  test("cannot set as clauses for bindings", () => {
    const prog = `
    forall Set x; Set y where y := Baz(x) as foo {}
    `;
    const parsed = parseStyle(prog);
    if (parsed.isErr()) {
      expect(parsed.error.message)
        .toEqual(`Error: Syntax error at line 2 col 43:

      forall Set x; Set y where y := Baz(x) as
                                            ^
Unexpected as token: "as". Instead, I was expecting to see one of the following:

    ws token
    nl token
    ";"
    comment token
    multiline_comment token
    "with"
    "{"
`);
    } else {
      throw Error("expected parse error");
    }
  });
  test("cannot set subVars as aliases", () => {
    const prog = "forall Set x; Set y where IsSubset(x,y) as `A` {}";
    const parsed = parseStyle(prog);
    if (parsed.isErr()) {
      expect(parsed.error.message)
        .toEqual(`Error: Syntax error at line 1 col 44:

  forall Set x; Set y where IsSubset(x,y) as \`
                                             ^
Unexpected tick token: "\`". Instead, I was expecting to see one of the following:

    ws token
    identifier token
`);
    } else {
      throw Error("expected parse error");
    }
  });

  test("label field check", () => {
    const prog = `
forall Set A, B
where IsSubset(A, B); A has math label; B has text label {

}
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
    const whereClauses = results[0].items[0].header.where.contents;
    expect(whereClauses[1].name.contents.value).toEqual("A");
    expect(whereClauses[1].field.value).toEqual("label");
    expect(whereClauses[1].fieldDescriptor).toEqual("MathLabel");
    expect(whereClauses[2].name.contents.value).toEqual("B");
    expect(whereClauses[2].field.value).toEqual("label");
    expect(whereClauses[2].fieldDescriptor).toEqual("TextLabel");
  });
});

describe("Block Grammar", () => {
  test("empty block with comments and blank lines", () => {
    const prog = `
forall Set A, B; Map f {

  -- comments




}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("single statement", () => {
    const prog = `
forall Set A, B; Map f {
  delete A.arrow.center
}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("delete statements with field, property paths", () => {
    const prog = `
forall Set A, B; Map f {
  delete A.arrow.center
  delete B.arrow
  delete localx
}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("line comments among statements", () => {
    const prog = `
forall Set A, B; Map f {
  -- beginning comment
  delete A.arrow.center
  -- between comment
  delete B.arrow
  delete localx
  -- end of block comment
}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("line comments after statements", () => {
    const prog = `
forall Set A, B; Map f {
  delete A.arrow.center -- end of statement comment
  -- between comment
  delete B.arrow
-- delete C.arrow
  delete localx -- end of block comment
}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("path assign with expressions", () => {
    const prog = `
forall Set B {
  -- property paths
  A.circle.boolProp = true
  A.circle.boolProp1 = false
  A.circle.strProp = "ABCdef1243_dfds&(*($#@"
}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
});

describe("Expr Grammar", () => {
  test("layering expr", () => {
    const prog = `
const {
  -- w/ optional keyword
  \`C\`.layering1 = layer A.circ above B.circ
  layering2 = layer B.circ below C.circ
  layering3 = layer B.circ below C.circ
  layering3 = layer B.circ below C.circ, D.circ
  layering4 = layer B.circ above C.circ, D.circ
  -- w/o optional keyword
  A.layering5 = A.circ above B.circ
  layering6 = B.circ below C.circ
  \`B\`.layering7 = layer B.circ above C.circ, D.circ
  layering8 = layer B.circ below C.circ, D.circ
}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
  test("string expr", () => {
    const prog = `
const {
  -- plain
  \`C\`.strin = "abs1232189y790yh97dasyhfda7fhnasopufn9"
  -- unicode
  chn = "penrose真好玩"
  -- escape char
  \`C\`.newline = "\\n"
  \`C\`.newline = "first line\\nnextline\\ttabbed"
  -- TODO: test slash
}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
    const ast = results[0];
    const stringAssign = ast.items[0].block.statements[0];
    if (
      stringAssign.tag === "PathAssign" &&
      stringAssign.value.tag === "StringLit"
    ) {
      expect(stringAssign.value.contents).toEqual(
        "abs1232189y790yh97dasyhfda7fhnasopufn9",
      );
    } else throw Error("First stmt is not an assignment to string");
  });

  test("floating point expr", () => {
    const prog = `
const {
  -- varying
  v1 = ?
  v2 = ensure near(?, 4.2, .5)
  -- int
  f1 = 42
  f2 = 424242424242
  -- decimals
  -- d1 = .42
  d2 = 420.
  d3 = 04350. -- error?
  -- exp
  e1 = 0.0314E+2
  e2 = 314e-2
  -- function
  A.func = comp("some string", 1.347e-2)
}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("computation functions expr", () => {
    const prog = `
const {
  -- no arg
  B.fn = compute(    )
  -- literals
  A.fn = compute("string1", true, "string\\n", false)
}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("objective and constraint expr", () => {
    const prog = `
const {
  -- encourage
  A.fn = encourage obj("string1", true, "string\\n", false)
  A.fn2 = encourage obj( a, b )
  encourage a == b
  encourage a + a / b > b*b
  encourage MathPI()*4 > abs(sqrt(b))
  encourage a < b
  encourage a > b
  -- ensure
  A.fn = ensure obj("string1", true, "string\\n", false)
  ensure a == b
  ensure a < b
  ensure a > b
  -- shape should be processed as keyword
  B.fn = ensure same( A.shape.prop , B.shape  )
  localVar = ensure same( A.shp , B.shp  )
}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("GPI constructor expr", () => {
    const prog = `
const {
  -- literals
  A.circle = Circle -- C-style braces
  { -- comment begin
    strokeStyle: "dashed"
    -- comment between lines
    beautiful: true --comment inline
    center: B.circle.center
    -- comment end
  }
  -- empty
  \`A\`.circle = Circle {  }
  A.circle = Circle {
    --- comments

  }
  -- venn
  p.icon = Circle {
    strokeWidth : 0.0
    color : rgba(0.0, 0.0, 0.0, 1.0)
    r : 3.0
  }
}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("styvar expr", () => {
    const prog = `
      forall A a {
        x = numberof a
        y = listof a from a
        z = nameof a
      }
    `;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("arithmetic expr", () => {
    const prog = `
const {
  -- pos/neg numbers
  float pn1 = -5.2
  pn2 = -3.14532e+2
  pn3 = 1 + (-3.14) / 3.0 * (-2.0)
  -- nesting and parens
  n1 = (1.0)
  n2 = (1.0 + .2)
  float n3 = 1.0 + 2.0 / .3
  -- plus/minus
  p1 = 1.0 - 2.0
  p2 = 1.0 + "string" -- should still parse
  p3 = 1.0 + ?
  float p4 = 1.0 + 2.0 - 3.0 + 4.0
  -- mul/div
  m1 = 1.0 / 2.0
  m2 = 1.0 * "string" -- should still parse
  m3 = 1.0 / ?
  m3 = 1.0 * ?
  m4 = 1.0 * 2.0 / 3.0 / 4.0
  -- exp
  e1 = 1.0^5 + 5.0^ 3
  e1 = 1.0 ^ (5 + 8) + 5.0 ^3
  -- unary op
  u2 = -pn2
  u1 = -A.shape.x + (-m1) * 5.0
}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("list expr", () => {
    const prog = `
testing {
  a = [1]
  a = [1  ]
  a = ["a", 1, 2,3,4    ,5  ]
}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("tuple expr", () => {
    const prog = `
testing {
  w = {"thing1", "thing2"}
  x = { 1, {2, comp( 5.0, x.shape.y) } }
  y = {{ "string", {2, x.shape.color} }, -2.}
  z = { "thing1", false}
}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("vector arith expr", () => {
    const prog = `
testing {
  m = (a, (-1., 2.))
  v = (a + (2, 900)) / (4.0 + 3)
}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("vector/matrix literal expr", () => {
    const prog = `
testing {
  m1 = ((1., 2.), (3., 4 ), ( 5., 6.))
  -- NOTE: this will parse. It's up to the checker to determine the type
  m2 = (1, (3., 4 ), "string", (4, 5, (-6)))
  m3 = ((42.))
  m4 = (x.shape.y, ({2, 3.}, localVar ), "string", (4, 5, (-6)))
}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });

  test("vector/matrix access expr", () => {
    const prog = `
testing {
  mat3x3 a1 = vec[42]
  vec3 a2 = vec[12][34]
  vec2 a3 = a[1] + a[0]
  vec2[] a4 = m [1][0] + m[c] [b]
  a5 = comp(x[1][b])
  a6 = A.shape.vec[comp(a1[12][34])][a2[56]]

}`;
    const { results } = parser.feed(prog);
    sameASTs(results);
  });
});
