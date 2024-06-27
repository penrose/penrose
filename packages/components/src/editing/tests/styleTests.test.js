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
  hasNoErrors,
  testNamespaceProps,
  testNamespaces,
  testStyleAutocomplete,
} from "./testUtils";

describe("Parser", () => {
  test("empty", () => {
    let input = "";
    expect(hasNoErrors(parser, input)).toBe(true);
  });
});

describe("Namespace Caching", () => {
  test("empty namespaces", () => {
    let input = `
    namespace1 {
    }
    namespace2 {
    }`;
    let expected = ["namespace1", "namespace2"];
    expect(testNamespaces(input, expected)).toBe(true);
  });
  test("has properties", () => {
    let input = `
    colors {
        color1 = #000000
        color2 = #000002
        color3 = #000004
    }`;
    let expected = ["color1", "color2", "color3"];
    expect(testNamespaceProps(input, "colors", expected)).toBe(true);
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
    let expected_names = ["colors", "global"];
    let expected_colors = ["lightGray", "mediumGray", "darkGray"];
    let expected_globals = ["tableWidth", "tableHeight", "boxPadding"];

    expect(testNamespaceProps(input, "colors", expected_colors)).toBe(true);
    expect(testNamespaceProps(input, "global", expected_globals)).toBe(true);
    expect(testNamespaces(input, expected_names)).toBe(true);
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
    expect(
      await testStyleAutocomplete(
        input,
        "",
        Object.keys(shapeDefns["Circle"]),
        2,
      ),
    ).toBe(true);
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
    expect(
      await testStyleAutocomplete(
        input,
        "",
        Object.keys(shapeDefns["Equation"]),
        2,
      ),
    ).toBe(true);
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

    expect(
      await testStyleAutocomplete(input, "", ["colors", "globals"], 1, true),
    ).toBe(true);
  });

  test("Namespace properties 1", async () => {
    const input =
      namespacesTestProg +
      ` forall Point p {
            a = colors.}`;

    expect(
      await testStyleAutocomplete(input, "", ["lightBlue", "green"], 1),
    ).toBe(true);
  });

  test("Namespace properties 2", async () => {
    const input =
      namespacesTestProg +
      ` forall Point p {
            a = colors.}`;

    expect(
      await testStyleAutocomplete(input, "", ["lightBlue", "green"], 1),
    ).toBe(true);
  });

  test("Namespace properties 3", async () => {
    const input =
      namespacesTestProg +
      ` forall Point p {
            a = globals.}`;

    expect(
      await testStyleAutocomplete(input, "", ["ballRadius", "numBalls"], 1),
    ).toBe(true);
  });

  test("Namespace properties 4", async () => {
    const input =
      namespacesTestProg +
      ` forall Point p {
            a = globals.b}`;

    expect(
      await testStyleAutocomplete(input, "", ["ballRadius", "numBalls"], 1),
    ).toBe(true);
  });

  test("Selector with", async () => {
    const input = `forall Set x wi`;

    expect(await testStyleAutocomplete(input, "", selectorHeaderKws)).toBe(
      true,
    );
  });

  test("Selector where", async () => {
    const input = `forall Set x; Set y; with Set z wh`;

    expect(await testStyleAutocomplete(input, "", selectorHeaderKws)).toBe(
      true,
    );
  });

  test("Header Kws", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    f`;

    expect(await testStyleAutocomplete(input, "", styleHeaderKws)).toBe(true);
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

    expect(await testStyleAutocomplete(input, "", styleHeaderKws)).toBe(true);
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

    expect(
      await testStyleAutocomplete(input, "", computationFns, 1, true),
    ).toBe(true);
  });
  test("Binary Expressions", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    forall Element A where IsCircular(A) {
        a = 5 + c}`;

    expect(
      await testStyleAutocomplete(input, "", computationFns, 1, true),
    ).toBe(true);
  });

  test("Assignment Exprs include shape names", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    forall Element A where IsCircular(A) {
        shape B = C}`;

    expect(await testStyleAutocomplete(input, "", shapeNames, 1, true)).toBe(
      true,
    );
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

    expect(await testStyleAutocomplete(input, "", constraints, 1, true)).toBe(
      true,
    );
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

    expect(await testStyleAutocomplete(input, "", constraints, 1, true)).toBe(
      true,
    );
  });

  test("Type names suggested in body of selector", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    forall Element A where IsCircular(A) {
        sca}`;

    expect(await testStyleAutocomplete(input, "", typeNamesArr, 1, true)).toBe(
      true,
    );
  });

  test("Type names suggested in body of collector", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    collect Balls b into bs {
        sca}`;

    expect(await testStyleAutocomplete(input, "", typeNamesArr, 1, true)).toBe(
      true,
    );
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

    expect(
      await testStyleAutocomplete(
        input,
        exampleDomain,
        ["Set", "Element", "Ball"],
        0,
        true,
      ),
    ).toBe(true);
  });

  test("Domain defined types in selector header ; sep", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    forall Set a; B`;

    expect(
      await testStyleAutocomplete(
        input,
        exampleDomain,
        ["Set", "Element", "Ball"],
        0,
        true,
      ),
    ).toBe(true);
  });

  test("Domain defined types in collector header", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    collect Ba`;

    expect(
      await testStyleAutocomplete(
        input,
        exampleDomain,
        ["Set", "Element", "Ball"],
        0,
        true,
      ),
    ).toBe(true);
  });

  test("Domain defined types in with clause", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    collect Balls b into bs with E`;

    expect(
      await testStyleAutocomplete(
        input,
        exampleDomain,
        ["Set", "Element", "Ball"],
        0,
        true,
      ),
    ).toBe(true);
  });

  test("Domain defined types repetable collector", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    collect repeatable Bal`;

    expect(
      await testStyleAutocomplete(
        input,
        exampleDomain,
        ["Set", "Element", "Ball"],
        0,
        true,
      ),
    ).toBe(true);
  });

  test("Collector repeatable", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    collect re`;

    expect(
      await testStyleAutocomplete(
        input,
        exampleDomain,
        ["repeatable"],
        0,
        true,
      ),
    ).toBe(true);
  });

  test("Collector into", async () => {
    const input = `canvas {
    width = 400
    height = 400
    }
    
    collect Ball b in`;

    expect(
      await testStyleAutocomplete(input, exampleDomain, ["into"], 0, true),
    ).toBe(true);
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

    expect(
      await testStyleAutocomplete(input, exampleDomain, ["into"], 0, true),
    ).toBe(true);
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

    expect(
      await testStyleAutocomplete(
        input,
        exampleDomainWithPredicates,
        ["Group", "Fill", "Rotate"],
        0,
        true,
      ),
    ).toBe(true);
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

    expect(
      await testStyleAutocomplete(
        input,
        exampleDomainWithPredicates,
        ["Group", "Fill", "Rotate"],
        0,
        true,
      ),
    ).toBe(true);
  });
});
