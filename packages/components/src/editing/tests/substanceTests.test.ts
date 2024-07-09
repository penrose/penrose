import { describe, expect, test } from "vitest";
import { getSubstanceCache } from "../hooks/substance/getSubstanceCache";
import { substanceKws } from "../hooks/substance/substanceAutocomplete";
import { parser } from "../parser/substance/substance";
import {
  constructSubstanceCacheObj,
  hasNoErrors,
  testSubstanceAutocomplete,
} from "./testUtils";

describe("Parser", () => {
  test("empty", () => {
    let prog = "";
    hasNoErrors(parser, prog);
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

    hasNoErrors(parser, prog);
  });

  test("trailing comment", () => {
    const prog = `
    Set A
    Set B
    Set C
    Set D
    -- Set E`;
    hasNoErrors(parser, prog);
  });
});

describe("statements", () => {
  test("iset 1", () => {
    // Numbers aren't showing in the parse tree
    const prog = `Set A for i in [0, 10]`;
    hasNoErrors(parser, prog);
  });
  test("iset 2", () => {
    // Numbers aren't showing in the parse tree
    const prog = `Set A for i in [0, 10], j in [1, 5]`;
    hasNoErrors(parser, prog);
  });
  test("indexed constructor 1", () => {
    // Numbers aren't showing in the parse tree
    const prog = `Set A := MakeSet(hello_j) for j in [0, 20]`;
    hasNoErrors(parser, prog);
  });

  test("indexed constructor 2", () => {
    // Numbers aren't showing in the parse tree
    const prog = `Let B := Set(hello_world) for abc in [80, 70]`;
    hasNoErrors(parser, prog);
  });

  test("indexed predicate 1", () => {
    // Numbers aren't showing in the parse tree
    const prog = `Edge(a_i, a_j) for i in [0, 20], j in [20, 30] where 
        i + 1 == j && j + 1 == i 
        || !(j == 1 && y == 2)`;
    hasNoErrors(parser, prog);
  });

  test("indexed predicate 2", () => {
    // Numbers aren't showing in the parse tree
    const prog = `Edge(v_i, v_i) for i in [0, 20] where 20 != 20`;
    hasNoErrors(parser, prog);
  });

  test("indexed labels 1", () => {
    // Numbers aren't showing in the parse tree
    const prog = `Label x_i "abcde" for i in [0, 10]`;
    hasNoErrors(parser, prog);
  });

  test("indexed labels 2", () => {
    // Numbers aren't showing in the parse tree
    const prog = `Label y $abc$ for j in [0, 15]`;
    hasNoErrors(parser, prog);
  });

  test("decl and decl list", () => {
    // Numbers aren't showing in the parse tree
    const prog = `Set A
        Map f, g, h`;
    hasNoErrors(parser, prog);
  });

  test("label decl", () => {
    // Numbers aren't showing in the parse tree
    const prog = `Set A, B, C
        Label A $\\vec{A}$
        Label B $B_1$`;
    hasNoErrors(parser, prog);
  });

  test("no label decl", () => {
    // Numbers aren't showing in the parse tree
    const prog = `Set A, B, C
        NoLabel A
        NoLabel B, C`;
    hasNoErrors(parser, prog);
  });

  test("auto label decl", () => {
    // Numbers aren't showing in the parse tree
    const prog = `Set A, B, C
        AutoLabel All
        AutoLabel B, C
        NoLabel B, C`;
    hasNoErrors(parser, prog);
  });

  test("bind and exprs", () => {
    // Numbers aren't showing in the parse tree
    const prog = `Set A, B, C
        Point p1, p2
        C := Intersection(A, B)`;
    hasNoErrors(parser, prog);
  });

  test("predicates", () => {
    // Numbers aren't showing in the parse tree
    const prog = `Set A, B, C
        Subset(A, B)`;
    hasNoErrors(parser, prog);
  });

  test("numbers and strings", () => {
    // Numbers aren't showing in the parse tree
    const prog = ` Set A
    Contains(A, 1)
    Contains(B, "never gonna give you up")
    Contains(C, 3.0)`;
    hasNoErrors(parser, prog);
  });
});

describe("Caching", () => {
  test("ids single line", () => {
    const input = `Set A, B, C, D, E, F, G
        Disjoint(F, G)
        Disjoint(B, C)

        AutoLabel All`;
    expect(getSubstanceCache(input)).toStrictEqual(
      constructSubstanceCacheObj(["A", "B", "C", "D", "E", "F", "G"]),
    );
  });

  test("ids many lines", () => {
    const input = `Element g1
Element g2
Element g3
Element g4
Element g5
Element g6
Element g7
Element g8`;

    expect(getSubstanceCache(input)).toStrictEqual(
      constructSubstanceCacheObj(
        Array.from({ length: 8 }, (_, i) => `g${i + 1}`),
      ),
    );
  });

  test("ids with Let assignment", () => {
    const input = `-- outer part of "A"
Point a1, a2, a3, a4
Let a5 := Segment( a1, a2 )
Let a6 := Segment( a2, a3 )
`;
    expect(getSubstanceCache(input)).toStrictEqual(
      constructSubstanceCacheObj(
        Array.from({ length: 6 }, (_, i) => `a${i + 1}`),
      ),
    );
  });

  test("ids with assignment", () => {
    const input = `
    LineSegment t1 := Line(a, b)
    LineSegment t2 := Line(a, b)
    `;

    expect(getSubstanceCache(input)).toStrictEqual(
      constructSubstanceCacheObj(["t1", "t2"]),
    );
  });

  test("ids ignores indexed statements", () => {
    const input = "Vector v_i for i in [0, 10]";

    expect(getSubstanceCache(input)).toStrictEqual(
      constructSubstanceCacheObj([]),
    );
  });

  test("caching runtime small", () => {
    const input = `
Domain U

Point x0
Ball B0 := ballAround( x0 )
Point x1 := sampleBoundary( B0 )
Ball B1 := ballAround( x1 )
Point x2 := sampleBoundary( B1 )
Ball B2 := ballAround( x2 )
Point x3 := sampleBoundary( B2 )
Ball B3 := ballAround( x3 )
Point x4 := sampleBoundary( B3 )
Ball B4 := ballAround( x4 )

Label U  $\Omega$
Label x0 $x_0$
Label x1 $x_1$
Label x2 $x_2$
Label x3 $\ldots$
Label x4 $x_k$`;

    const start = performance.now();
    getSubstanceCache(input);
    const end = performance.now();
    const executionTime = end - start;

    console.log(`Substance Cache Execution Time: ${executionTime} ms`);
    expect(executionTime).toBeLessThan(10);
  });

  test("caching runtime large", () => {
    // Insertion sort substance
    const input = `--Autogenerated array substance
Array a1
Element g1
Element g2
Element g3
Element g4
Element g5
Element g6
Element g7
inArray(g1,a1,0)
inArray(g2,a1,1)
inArray(g3,a1,2)
inArray(g4,a1,3)
inArray(g5,a1,4)
inArray(g6,a1,5)
inArray(g7,a1,6)
Group p1
inGroup(g1,p1)
highlight(g2)
Pointer r1 := Arc(g2,g1)
Label g1 $42$
Label g2 $8$
Label g3 $14$
Label g4 $94$
Label g5 $4$
Label g6 $22$
Label g7 $74$
Label a1 $Unsorted$

Array a2
Element g8
Element g9
Element g10
Element g11
Element g12
Element g13
Element g14
inArray(g8,a2,0)
inArray(g9,a2,1)
inArray(g10,a2,2)
inArray(g11,a2,3)
inArray(g12,a2,4)
inArray(g13,a2,5)
inArray(g14,a2,6)
Group p2
inGroup(g8,p2)
inGroup(g9,p2)
highlight(g10)
Pointer r2 := Arc(g10,g9)
Label g8 $8$
Label g9 $42$
Label g10 $14$
Label g11 $94$
Label g12 $4$
Label g13 $22$
Label g14 $74$
Label a2 $Stage1$

Array a3
Element g15
Element g16
Element g17
Element g18
Element g19
Element g20
Element g21
inArray(g15,a3,0)
inArray(g16,a3,1)
inArray(g17,a3,2)
inArray(g18,a3,3)
inArray(g19,a3,4)
inArray(g20,a3,5)
inArray(g21,a3,6)
Group p3
inGroup(g15,p3)
inGroup(g16,p3)
inGroup(g17,p3)
highlight(g18)
Label g15 $8$
Label g16 $14$
Label g17 $42$
Label g18 $94$
Label g19 $4$
Label g20 $22$
Label g21 $74$
Label a3 $Stage2$

Array a4
Element g22
Element g23
Element g24
Element g25
Element g26
Element g27
Element g28
inArray(g22,a4,0)
inArray(g23,a4,1)
inArray(g24,a4,2)
inArray(g25,a4,3)
inArray(g26,a4,4)
inArray(g27,a4,5)
inArray(g28,a4,6)
Group p4
inGroup(g22,p4)
inGroup(g23,p4)
inGroup(g24,p4)
inGroup(g25,p4)
highlight(g26)
Pointer r3 := Arc(g26,g22)
Label g22 $8$
Label g23 $14$
Label g24 $42$
Label g25 $94$
Label g26 $4$
Label g27 $22$
Label g28 $74$
Label a4 $Stage3$

Array a5
Element g29
Element g30
Element g31
Element g32
Element g33
Element g34
Element g35
inArray(g29,a5,0)
inArray(g30,a5,1)
inArray(g31,a5,2)
inArray(g32,a5,3)
inArray(g33,a5,4)
inArray(g34,a5,5)
inArray(g35,a5,6)
Group p5
inGroup(g29,p5)
inGroup(g30,p5)
inGroup(g31,p5)
inGroup(g32,p5)
inGroup(g33,p5)
highlight(g34)
Pointer r4 := Arc(g34,g32)
Label g29 $4$
Label g30 $8$
Label g31 $14$
Label g32 $42$
Label g33 $94$
Label g34 $22$
Label g35 $74$
Label a5 $Stage4$

Array a6
Element g36
Element g37
Element g38
Element g39
Element g40
Element g41
Element g42
inArray(g36,a6,0)
inArray(g37,a6,1)
inArray(g38,a6,2)
inArray(g39,a6,3)
inArray(g40,a6,4)
inArray(g41,a6,5)
inArray(g42,a6,6)
Group p6
inGroup(g36,p6)
inGroup(g37,p6)
inGroup(g38,p6)
inGroup(g39,p6)
inGroup(g40,p6)
inGroup(g41,p6)
highlight(g42)
Pointer r5 := Arc(g42,g41)
Label g36 $4$
Label g37 $8$
Label g38 $14$
Label g39 $22$
Label g40 $42$
Label g41 $94$
Label g42 $74$
Label a6 $Stage5$

Array a7
Element g43
Element g44
Element g45
Element g46
Element g47
Element g48
Element g49
inArray(g43,a7,0)
inArray(g44,a7,1)
inArray(g45,a7,2)
inArray(g46,a7,3)
inArray(g47,a7,4)
inArray(g48,a7,5)
inArray(g49,a7,6)
Label g43 $4$
Label g44 $8$
Label g45 $14$
Label g46 $22$
Label g47 $42$
Label g48 $74$
Label g49 $94$
Label a7 $Sorted$`;
    const start = performance.now();
    getSubstanceCache(input);
    const end = performance.now();
    const executionTime = end - start;

    console.log(`Substance Cache Execution Time: ${executionTime} ms`);
    expect(executionTime).toBeLessThan(20);
  });
});

// Narrowing of results is handled by a separate Codemirror extension
// End back tick for input strings should be same line as the last character
// because test function cursor position placing is sensitive to whitespace
describe("Autocomplete", () => {
  test("KWs Empty Domain", async () => {
    const input = `L`;

    await testSubstanceAutocomplete(input, "", substanceKws);
  });

  test("KWs 1", async () => {
    const input = `Element A, B, C
    E`;

    const domainProg = `type Element
    type Point`;

    await testSubstanceAutocomplete(
      input,
      domainProg,
      substanceKws.concat(["Element", "Point"]),
    );
  });

  const ref_domainProg = `type Element
  type Point
  predicate Circle(Point, Point)
  predicate Ball(Element, Point)
  function CreateBalls(Point, Point, Point) -> Element
  constructor Bisect(Point) -> Element`;

  test("KWs 2", async () => {
    const input = `Element A, B, C
    Point p1, p2
    Circle(p1, p2)
    B`;

    await testSubstanceAutocomplete(
      input,
      ref_domainProg,
      substanceKws.concat(["Element", "Point", "Circle", "Ball"]),
    );
  });

  test("AutoLabel", async () => {
    const input = `Element A, B, C
    Point p1, p2
    Circle(p1, p2)
    AutoLabel A`;

    await testSubstanceAutocomplete(input, ref_domainProg, [
      "All",
      "A",
      "B",
      "C",
      "p1",
      "p2",
    ]);
  });

  test("where", async () => {
    const input = `Element A, B, C
    Point p_i for i in [0,5] w`;

    await testSubstanceAutocomplete(input, ref_domainProg, ["where"]);
  });

  test("for in type index", async () => {
    const input = `Point A, B, C
    Point p_i f`;

    await testSubstanceAutocomplete(input, ref_domainProg, ["for"]);
  });

  test("for in predicate index", async () => {
    const input = `Point A, B, C
    Circle(p_i, p_j) fo`;

    await testSubstanceAutocomplete(input, ref_domainProg, ["for"]);
  });

  test("suggest functions and constructors 1", async () => {
    const input = `Point A, B, C
    Circle(A, B) fo
    D := C`;

    await testSubstanceAutocomplete(input, ref_domainProg, [
      "CreateBalls",
      "Bisect",
    ]);
  });

  test("suggest functions and constructors 2", async () => {
    const input = `Point A, B, C
    Circle(A, B)
    Element D := C`;

    await testSubstanceAutocomplete(input, ref_domainProg, [
      "CreateBalls",
      "Bisect",
    ]);
  });

  test("suggest functions and constructors 3", async () => {
    const input = `Point A, B, C
    Circle(A, B)
    Let D := C`;

    await testSubstanceAutocomplete(input, ref_domainProg, [
      "CreateBalls",
      "Bisect",
    ]);
  });

  // Offset by 1 to put cursor after x rather than after )
  test("id first in pred params", async () => {
    const input = `Point x1, x2, x3
    Circle(x)`;

    await testSubstanceAutocomplete(
      input,
      ref_domainProg,
      ["x1", "x2", "x3"],
      1,
    );
  });

  test("id after comma in pred params", async () => {
    const input = `Point x1, x2, x3
    Circle(x1, x2, x)`;

    await testSubstanceAutocomplete(
      input,
      ref_domainProg,
      ["x1", "x2", "x3"],
      1,
    );
  });

  test("id in function param", async () => {
    const input = `Point x1, x2, x3
    Point x4 := CreateBalls(x1, x)`;

    await testSubstanceAutocomplete(
      input,
      ref_domainProg,
      ["x1", "x2", "x3", "x4"],
      1,
    );
  });

  test("id in constructor param", async () => {
    const input = `Point x1, x2, x3
    Point x4 := Bisect(x1)
    Element x5 := CreateBalls(x1, x)`;

    await testSubstanceAutocomplete(
      input,
      ref_domainProg,
      ["x1", "x2", "x3", "x4", "x5"],
      1,
    );
  });

  test("id in NoLabel", async () => {
    const input = `Point x1, x2, x3
    Point x4 := Bisect(x1)
    Element x5 := CreateBalls(x1, x2, x3)
    NoLabel x1, x2, x`;

    await testSubstanceAutocomplete(input, ref_domainProg, [
      "x1",
      "x2",
      "x3",
      "x4",
      "x5",
    ]);
  });
});
