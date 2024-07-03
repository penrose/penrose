import { describe, expect, test } from "vitest";
import { getShapeDefs } from "../hooks/hooksUtils";
import {
  getComputationFns,
  getConstraints,
  getNamespaceDict,
  getShapeNames,
  selectorHeaderKws,
  styleHeaderKws,
  typeNamesArr,
} from "../hooks/style/styleAutocompleteUtils";
import { parser } from "../parser/style/style";
import {
  hasErrors,
  hasNoErrors,
  testLayoutStages,
  testNamespaceProps,
  testNamespaces,
  testStyleAutocomplete,
} from "./testUtils";

describe("Parser", () => {
  test("empty", () => {
    let prog = "";
    hasNoErrors(parser, prog);
  });

  test("unbalanced curly", () => {
    const prog = `
    forall Set x {
      x.shape = Circle {
  }
    `;
    hasErrors(parser, prog);
  });

  test("type keyword check", () => {
    const prog = `
const {
  A.shape = Circle {}
  B.icon.x = 2.5
}
    `;
    hasNoErrors(parser, prog);
  });
});

describe("Selector Grammar", () => {
  test("empty namespace block", () => {
    const prog = "const { }";
    hasNoErrors(parser, prog);
  });

  test("empty selector block", () => {
    const prog = "forall Set A{}";
    hasNoErrors(parser, prog);
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
    hasNoErrors(parser, prog);
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
    hasNoErrors(parser, prog);
  });

  test("empty pred", () => {
    const prog = `
forall Set A, B, C
where Subset(   );
{ }`;
    hasNoErrors(parser, prog);
  });

  test("with, where, and as", () => {
    const prog = `
forall Set A, B; Map f
with Set C, D; Map \`g\`
where C := intersect ( A, B ) ; Subset( A, B ); Subset( B, C); Intersect (   )
as Const
{ }`;
    hasNoErrors(parser, prog);
  });

  test("collector", () => {
    const p1 = `collect Element e into es {}`;
    hasNoErrors(parser, p1);

    const p2 = `collect Element e
    into es where In(e, s) with Set s {}`;
    hasNoErrors(parser, p2);

    const p3 = `collect Element e
    into es where In(e, s) foreach Set s {}`;
    hasNoErrors(parser, p3);

    const p4 = `collect Element e
    into es where In(e, s) with Set s
    foreach OtherThing o {}`;
    hasNoErrors(parser, p4);

    const p5 = `collect Element e
    into es foreach OtherThing o where In(e, s) 
    with Set s {}`;
    hasNoErrors(parser, p5);

    const p6 = `collect Element e
    into es 
    with Set s foreach OtherThing o where In(e, s) {} `;
    hasNoErrors(parser, p6);
  });

  test("multiple as clauses for predicates", () => {
    const prog = `
forall Set A, B, C, D
where Subset(A, B) as foo; Subset(B,C) as bar; Union(C,D) as yeet;
{
  foo.arrow = Arrow{}
  yeet.circle = Circle{}
  bar.square = Square{}
}`;
    hasNoErrors(parser, prog);
  });

  test("alias general test", () => {
    const prog =
      "\
  forall Atom a1; Atom a2 \
  where Bond(a1, a2) as b {}";
    hasNoErrors(parser, prog);
  });

  test("alias expression location test", () => {
    // why is this failing bro
    const prog =
      "\
forall Set A, B, C; Map f \
with Set C, D; Map `g` \
where C := intersect ( A, B ) ; Subset( A, B ) as SubsetAB; Subset( B, C) as nested_Subset; Intersect (   ){}\
";
    hasNoErrors(parser, prog);
  });

  test("alias expression location test, weird spacing", () => {
    // why is this failing bro
    const prog =
      "\
forall Set A, B, C; Map f \
with Set C, D; Map `g` \
where C := intersect ( A, B ) ;\
 Subset( A, B ) \
 as SubsetAB; Subset( B, C) as nested_Subset; \
 Intersect (   )  as   ok {}\
";
    hasNoErrors(parser, prog);
  });

  test("cannot set as clauses for decls", () => {
    const prog = `forall Set A, B; Map f as Const -- should fail because decls can't be aliased
`;
    hasErrors(parser, prog);
  });

  test("cannot set as clauses for bindings", () => {
    const prog = `
    forall Set x; Set y where y := Baz(x) as foo {}
    `;
    hasErrors(parser, prog);
  });

  test("cannot set subVars as aliases", () => {
    const prog = "forall Set x; Set y where Subset(x,y) as `A` {}";
    hasErrors(parser, prog);
  });

  test("label field check", () => {
    const prog = `
forall Set A, B
where Subset(A, B); A has math label; B has text label {

}
    `;
    hasNoErrors(parser, prog);
  });

  test("literals in selectors", () => {
    const prog = `
    forall Set A, B
    where Contains(A, 1); NotContains(B, "hello world") {
    }`;
    hasNoErrors(parser, prog);
  });
});

test("empty block with comments and blank lines", () => {
  const prog = `
forall Set A, B; Map f {

  -- comments
}`;
  hasNoErrors(parser, prog);
});

test("single statement", () => {
  const prog = `
forall Set A, B; Map f {
  delete A.arrow.center
}`;
  hasNoErrors(parser, prog);
});

test("delete statements with field, property paths", () => {
  const prog = `
forall Set A, B; Map f {
  delete A.arrow.center
  delete B.arrow
  delete localx
}`;
  hasNoErrors(parser, prog);
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
  hasNoErrors(parser, prog);
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
  hasNoErrors(parser, prog);
});

test("path assign with expressions", () => {
  const prog = `
forall Set B {
  -- property paths
  A.circle.boolProp = true
  A.circle.boolProp1 = false
  A.circle.strProp = "ABCdef1243_dfds&(*($#@"
}`;
  hasNoErrors(parser, prog);
});

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
  hasNoErrors(parser, prog);
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
  hasNoErrors(parser, prog);
});

test("floating point expr", () => {
  const prog = `
const {
  -- varying
  v1 = ?
  v2 = ensure near(?, 4.2, .5)
  v3 = ?[1.2345]
  v4 = ? [ 1.2345 ]
  v5 = ?[ 12345]
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
  hasNoErrors(parser, prog);
});

test("computation functions expr", () => {
  const prog = `
const {
  -- no arg
  B.fn = compute(    )
  -- literals
  A.fn = compute("string1", true, "string\\n", false)
}`;
  hasNoErrors(parser, prog);
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
  hasNoErrors(parser, prog);
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
  -- euler
  p.icon = Circle {
    strokeWidth : 0.0
    color : rgba(0.0, 0.0, 0.0, 1.0)
    r : 3.0
  }
}`;
  hasNoErrors(parser, prog);
});

test("styvar expr", () => {
  const prog = `
      forall A a {
        x = numberof a
        y = listof a from a
        z = nameof a
      }
    `;
  hasNoErrors(parser, prog);
});

test("arithmetic expr", () => {
  const prog = `
const {
  -- pos/neg numbers
  pn1 = -5.2
  pn2 = -3.14532e+2
  pn3 = 1 + (-3.14) / 3.0 * (-2.0)
  -- nesting and parens
  n1 = (1.0)
  n2 = (1.0 + .2)
  n3 = 1.0 + 2.0 / .3
  -- plus/minus
  p1 = 1.0 - 2.0
  p2 = 1.0 + "string" -- should still parse
  p3 = 1.0 + ?
  p4 = 1.0 + 2.0 - 3.0 + 4.0
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
  hasNoErrors(parser, prog);
});

test("list expr", () => {
  const prog = `
testing {
  a = [1]
  a = [1  ]
  a = ["a", 1, 2,3,4    ,5  ]
}`;
  hasNoErrors(parser, prog);
});

test("tuple expr", () => {
  const prog = `
testing {
  w = {"thing1", "thing2"}
  x = { 1, {2, comp( 5.0, x.shape.y) } }
  y = {{ "string", {2, x.shape.color} }, -2.}
  z = { "thing1", false}
}`;
  hasNoErrors(parser, prog);
});

test("vector arith expr", () => {
  const prog = `
testing {
  m = (a, (-1., 2.))
  v = (a + (2, 900)) / (4.0 + 3)
}`;
  hasNoErrors(parser, prog);
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
  hasNoErrors(parser, prog);
});

test("vector/matrix access expr", () => {
  const prog = `
testing {
  mat3x3 a1 = vec[42]
  vec3 a2 = vec[12][34]
  vec2 a3 = a[1] + a[0]
  vec2 a4 = m [1][0] + m[c] [b]
  a5 = comp(x[1][b])
  a6 = A.shape.vec[comp(a1[12][34])][a2[56]]

}`;
  hasNoErrors(parser, prog);
});

describe("Namespace Caching", () => {
  test("empty namespaces", () => {
    let input = `
    namespace1 {
    }
    namespace2 {
    }`;
    let expected = ["namespace1", "namespace2"];
    testNamespaces(input, expected);
  });
  test("has properties", () => {
    let input = `
    colors {
        color1 = #000000
        color2 = #000002
        color3 = #000004
    }`;
    let expected = ["color1", "color2", "color3"];
    testNamespaceProps(input, "colors", expected);
  });

  test("full prog", () => {
    let input = `
    canvas {
    width = 240
    height = 200
}

colors {
   lightGray = rgba( .8, .8, .8, 1. )
   mediumGray = rgba( .6, .6, .6, 1. )
   darkGray = rgba( .4, .4, .4, 1. )
}

global {
   scalar tableWidth = 180
   scalar tableHeight = 180

   scalar boxPadding = 2.0
}

-- draw each group element as a label for a row and column
forall Element g
{
   scalar m = match_id
   scalar n = match_total

   -- associate each group element with a value t ∈ [0,1]
   scalar g.t = m / n

   -- compute horizontal/vertical coordinates u/v for each element
   scalar g.u = ( g.t - 0.5 ) * global.tableHeight
   scalar g.v = ( (1.-g.t) - 0.5 ) * global.tableWidth
}
`;
    let expected_names = ["colors", "global", "canvas"];
    let expected_colors = ["lightGray", "mediumGray", "darkGray"];
    let expected_globals = ["tableWidth", "tableHeight", "boxPadding"];

    testNamespaceProps(input, "colors", expected_colors);
    testNamespaceProps(input, "global", expected_globals);
    testNamespaces(input, expected_names);
  });

  test("runtime 1", () => {
    const input = `

layout = [ shapeStage, labelStage ]

canvas {
   width = 306.66 -- ==230
   height = 200.00 -- ==150
}

--------------------------------------------------
-- Global constants ------------------------------
--------------------------------------------------

colors {
   color black = rgba(0,0,0,1)
   color white = rgba(1,1,1,1)
   color lightGray = rgba(.8,.8,.8,1)
   color clearGray = rgba(0,0,0,.2)
   color darkBlue = #1b1f8a
}

global {
   scalar toPt = 96/72 -- constant for converting sizes from pt to px

   -- dot style
   color dotColor = colors.black
   scalar dotRadius = toPt * 1.75
   scalar dotStroke = toPt * .75

   -- line and arrow style
   scalar lineWidth = toPt * 1
   scalar arrowLength = 25.0

   -- label style
   string labelStyle = "italic"
   string labelFamily = "Linux Libertine"
   color labelColor = #000
   string labelSize = "13.3333px" -- equivalent to 10pt (multiply by 96/72)
   scalar labelHeight = 10
   scalar labelDistance = 8.0

   -- layout parameters
   scalar padding = 2.0 -- amount of padding to prevent overlap
}

--------------------------------------------------
-- Points ----------------------------------------
--------------------------------------------------

forall Point p {

}

forall Point p
where p has label {
}

--------------------------------------------------
-- Vectors ---------------------------------------
--------------------------------------------------

forall Vector v {
}

forall Vector v
where v has label {

}

forall Vector v; Point p
where RootedAt(p,v) {
}

-- draw segment normal as a vector at its midpoint
forall Vector v; Segment s
where v := Normal(s) {
}

--------------------------------------------------
-- Segments --------------------------------------
--------------------------------------------------

forall Segment s {
}

forall Segment s; Point p; Point q
where s := Segment(p,q) {
}

forall Segment s
where s has label {
}

-- Oriented segments are given an orientation marker in
-- the middle, going from the first to second point used
-- to define the segment.
forall Segment s
where IsOriented( s ) {
}

forall Segment s
where IsDashed( s ) {
}

--------------------------------------------------
-- Angle markers --------------------------------
--------------------------------------------------

forall Angle a {
}

forall Angle a
where IsOriented( a ) {
}

forall Angle a; Point x,y,z
where a := InteriorAngle(x,y,z) {
}

forall Angle a; Triangle t {
}

--------------------------------------------------
-- Length markers --------------------------------
--------------------------------------------------

-- draw length markers as segments with perpendicular markers at the ends
forall Length l {

}

forall Length l; Point a; Point b
where l := LengthBetween(a,b) {
}

forall Length l; Segment s
where l := LengthOf(s) {
}

forall Length l
where l has label {

}

--------------------------------------------------
-- Triangles -------------------------------------
--------------------------------------------------

forall Triangle t
{
}

forall Triangle t; Point p1; Point p2; Point p3
where t := Triangle(p1, p2, p3)
{
}

forall Point p; Triangle t; Point q0; Point q1; Point q2
where InTri( p, t ); t := Triangle(q0, q1, q2) {
}`;

    const tree = parser.parse(input);
    const start = performance.now();
    getNamespaceDict(tree.topNode, input);
    const end = performance.now();
    const executionTime = end - start;

    console.log(`Style Namespace Cache Execution Time: ${executionTime} ms`);
    expect(executionTime).toBeLessThan(10);
  });
});

describe("Stage name Caching", () => {
  test("Stage names 1", async () => {
    const input = `layout = [ walkStage, nestStage, labelStage, legendStage ]

-- diagram dimensions (in px; multiply by 96/72 to convert to pt)
canvas {
   width = 200 -- ==150pt
   height = 200 -- ==150pt
}`;

    testLayoutStages(input, [
      "walkStage",
      "nestStage",
      "labelStage",
      "legendStage",
    ]);
  });

  test("Stage names 2", async () => {
    const input = `canvas {
   width = 200 -- ==150pt
   height = 200 -- ==150pt
}
   forall Set x {
  shape x.icon = Circle { }
  shape x.text = Equation {
    string : x.label
    fontSize : "32px"
  }
  ensure contains(x.icon, x.text)
  encourage norm(x.text.center - x.icon.center) == 0
  layer x.text above x.icon
}

layout = [A, B, C, D]
`;
    testLayoutStages(input, ["A", "B", "C", "D"]);
  });
});

describe("Autocomplete", () => {
  const shapeDefns = getShapeDefs();
  const shapeNames = getShapeNames(shapeDefns).map((cmp) => cmp.label.trim());
  const computationFns = getComputationFns().map((cmp) => cmp.label.trim());
  const constraints = getConstraints().map((cmp) => cmp.label.trim());

  test("ShapeProps Circle", async () => {
    const input = `canvas {
        width = 500
        height = 500 
        }
        
        forall Point p {
        Circle {
            s}}`;

    // Offset by 2 because of ending braces

    await testStyleAutocomplete(
      input,
      "",
      Object.keys(shapeDefns["Circle"]),
      2,
    );
  });

  test("ShapeProps Equation", async () => {
    const input = `canvas {
        width = 500
        height = 500 
        }
        
        forall Point p {
        Equation {
            s}}`;

    // Offset by 2 because of ending braces
    await testStyleAutocomplete(
      input,
      "",
      Object.keys(shapeDefns["Equation"]),
      2,
    );
  });

  const namespacesTestProg = `canvas {
        width = 500
        height = 500 
        }

        colors {
            lightBlue = #111111
            green = #111111
        }

        globals {
            ballRadius = 5
            numBalls = 2
        }`;

  test("Namespaces", async () => {
    const input =
      namespacesTestProg +
      ` forall Point p {
            a = c}`;

    await testStyleAutocomplete(input, "", ["colors", "globals"], 1, true);
  });

  test("Namespace properties 1", async () => {
    const input =
      namespacesTestProg +
      ` forall Point p {
            a = colors.}`;

    await testStyleAutocomplete(input, "", ["lightBlue", "green"], 1);
  });

  test("Namespace properties 2", async () => {
    const input =
      namespacesTestProg +
      ` forall Point p {
            a = colors.}`;

    await testStyleAutocomplete(input, "", ["lightBlue", "green"], 1);
  });

  test("Namespace properties 3", async () => {
    const input =
      namespacesTestProg +
      ` forall Point p {
            a = globals.}`;

    await testStyleAutocomplete(input, "", ["ballRadius", "numBalls"], 1);
  });

  test("Namespace properties 4", async () => {
    const input =
      namespacesTestProg +
      ` forall Point p {
            a = globals.b}`;

    await testStyleAutocomplete(input, "", ["ballRadius", "numBalls"], 1);
  });

  test("Selector with", async () => {
    const input = `forall Set x wi`;
    await testStyleAutocomplete(input, "", selectorHeaderKws);
  });

  test("Selector where", async () => {
    const input = `forall Set x; Set y; with Set z wh`;
    await testStyleAutocomplete(input, "", selectorHeaderKws);
  });

  test("Header Kws", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    f`;
    await testStyleAutocomplete(input, "", styleHeaderKws);
  });

  test("Header Kws following selector", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    forall Element A where IsCircular(A) {
        a = Circle {
            r: 5
        }
    }
    
    c`;
    await testStyleAutocomplete(input, "", styleHeaderKws);
  });

  test("Stage Names after ?", async () => {
    const input = `layout = [ stageA, stageB, stageC ]

-- diagram dimensions (in px; multiply by 96/72 to convert to pt)
canvas {
   width = 200 -- ==150pt
   height = 200 -- ==150pt
}

forall Points p {
scalar d = ? in s}`;

    await testStyleAutocomplete(input, "", ["stageA", "stageB", "stageC"], 1);
  });

  test("Stage Names in Objective", async () => {
    const input = `layout = [ stage1, stage2, stage3 ]

-- diagram dimensions (in px; multiply by 96/72 to convert to pt)
canvas {
   width = 200 -- ==150pt
   height = 200 -- ==150pt
}

forall Points p {
ensure norm(p) == 5 in s}`;

    await testStyleAutocomplete(input, "", ["stage1", "stage2", "stage3"], 1);
  });

  // Tests assume computation functions suggested in expressions.
  // Mainly checking that the expression path is triggered.
  // More robust tests could be added for expressions
  test("Assignment Expressions", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    forall Element A where IsCircular(A) {
        a = c}`;

    await testStyleAutocomplete(input, "", computationFns, 1, true);
  });
  test("Binary Expressions", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    forall Element A where IsCircular(A) {
        a = 5 + c}`;

    await testStyleAutocomplete(input, "", computationFns, 1, true);
  });

  test("Assignment Exprs include shape names", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    forall Element A where IsCircular(A) {
        shape B = C}`;
    await testStyleAutocomplete(input, "", shapeNames, 1, true);
  });

  test("Constraint fns after encourage", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    forall Element A where IsCircular(A) {
        scalar a = 5
        string b = "hello"
        encourage i}`;
    await testStyleAutocomplete(input, "", constraints, 1, true);
  });

  test("Constraint fns after ensure", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    forall Element A where IsCircular(A) {
        scalar a = 5
        string b = "hello"
        ensure i}`;
    await testStyleAutocomplete(input, "", constraints, 1, true);
  });

  test("Type names suggested in body of selector", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    forall Element A where IsCircular(A) {
        sca}`;
    await testStyleAutocomplete(input, "", typeNamesArr, 1, true);
  });

  test("Type names suggested in body of collector", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    collect Balls b into bs {
        sca}`;
    await testStyleAutocomplete(input, "", typeNamesArr, 1, true);
  });

  const exampleDomain = `type Set
  type Element
  type Ball`;

  test("Domain defined types in selector header", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    forall S`;

    await testStyleAutocomplete(
      input,
      exampleDomain,
      ["Set", "Element", "Ball"],
      0,
      true,
    );
  });

  test("Domain defined types in selector header ; sep", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    forall Set a; B`;

    await testStyleAutocomplete(
      input,
      exampleDomain,
      ["Set", "Element", "Ball"],
      0,
      true,
    );
  });

  test("Domain defined types in collector header", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    collect Ba`;

    await testStyleAutocomplete(
      input,
      exampleDomain,
      ["Set", "Element", "Ball"],
      0,
      true,
    );
  });

  test("Domain defined types in with clause", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    collect Balls b into bs with E`;

    await testStyleAutocomplete(
      input,
      exampleDomain,
      ["Set", "Element", "Ball"],
      0,
      true,
    );
  });

  test("Domain defined types repetable collector", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    collect repeatable Bal`;

    await testStyleAutocomplete(
      input,
      exampleDomain,
      ["Set", "Element", "Ball"],
      0,
      true,
    );
  });

  test("Selector repeatable", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    forall re`;

    await testStyleAutocomplete(input, exampleDomain, ["repeatable"], 0, true);
  });

  test("Collector repeatable", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    collect re`;

    await testStyleAutocomplete(input, exampleDomain, ["repeatable"], 0, true);
  });

  test("Collector into", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    collect Ball b in`;

    await testStyleAutocomplete(input, exampleDomain, ["into"], 0, true);
  });

  test("Collector into following selector", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }

    forall Ball a {
        scalar b = 15
    }
    
    collect Ball b in`;

    await testStyleAutocomplete(input, exampleDomain, ["into"], 0, true);
  });

  const exampleDomainWithPredicates = `type Set
  type Element
  type Ball
  predicate Group(Set a, Set b)
  predicate Fill(Ball a)
  predicate Rotate(Element a)
  `;

  test("Collector preds in where clause", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }

    forall Ball a {
        scalar b = 15
    }
    
    collect Ball b into bs where F`;

    await testStyleAutocomplete(
      input,
      exampleDomainWithPredicates,
      ["Group", "Fill", "Rotate"],
      0,
      true,
    );
  });

  test("Selector preds in where clause", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }

    forall Ball a {
        scalar b = 15
    }
    
    forall Set a; Element b; where F`;

    await testStyleAutocomplete(
      input,
      exampleDomainWithPredicates,
      ["Group", "Fill", "Rotate"],
      0,
      true,
    );
  });
});
