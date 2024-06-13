import {createTestParser} from "../utils.js"
import { describe, test } from "vitest";
import {parser} from './substance.js'

const testParser = createTestParser(parser)

// expected obtained by printing and then inspecting program vs output
describe("Common", () => {
    test("empty", () => {
        let input = ''
        let expected = 'Program'
        testParser(input, expected)
    })

    test("comments and white spaces", () => {
        const input = `
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
    
        const expected = `Program(LineComment,
            TypeApp(Identifier,Identifier,Identifier,Identifier,Identifier,Identifier,Identifier,Identifier),
            LineComment,
            BlockComment,
            LineComment,
            TypeApp(Identifier,Identifier),
            LineComment,
            Labeling(AutoLabel,All),
            BlockComment)`

        testParser(input, expected)
      });

      test("trailing comment", () => {
        const input = `
    Set A
    Set B
    Set C
    Set D
    -- Set E`;
        
        const expected = `Program(TypeApp(Identifier,Identifier),
            TypeApp(Identifier,Identifier),
            TypeApp(Identifier,Identifier),
            TypeApp(Identifier,Identifier),
            LineComment)`
    
        testParser(input, expected)
      });
})

describe("statements", () => {
    test("iset 1", () => {
        // Numbers aren't showing in the parse tree
        const input = `Set A for i in [0, 10]`
        const expected = `Program(TypeApp(Identifier,Identifier,
            IndexedStatement(for,Identifier,in,Number,Number)))`
        testParser(input, expected)
    })
    test("iset 2", () => {
        // Numbers aren't showing in the parse tree
        const input = `Set A for i in [0, 10], j in [1, 5]`
        const expected = `Program(TypeApp(Identifier,Identifier,
            IndexedStatement(for,
                Identifier,in,Number,Number,
                Identifier,in,Number,Number)))`
        testParser(input, expected)
    })
    test("indexed constructor 1", () => {
        // Numbers aren't showing in the parse tree
        const input = `Set A := MakeSet(hello_j) for j in [0, 20]`
        const expected = `Program(Fn_ConsApp(Identifier,Identifier,Identifier,ArgList(Identifier),
            IndexedStatement(for,Identifier,in,Number,Number)))`
        testParser(input, expected)
    })

    test("indexed constructor 2", () => {
        // Numbers aren't showing in the parse tree
        const input = `Let B := Set(hello_world) for abc in [80, 70]`
        const expected = `Program(Fn_ConsApp(Let,Identifier,Identifier,ArgList(Identifier),
            IndexedStatement(for,Identifier,in,Number,Number)))`
        testParser(input, expected)
    })

    test("indexed predicate 1", () => {
        // Numbers aren't showing in the parse tree
        const input = `Edge(a_i, a_j) for i in [0, 20], j in [20, 30] where 
        i + 1 == j && j + 1 == i 
        || !(j == 1 && y == 2)`
        const expected = `Program(PredicateApp(Identifier,ArgList(Identifier,Identifier),IndexedStatement(for,Identifier,in,Number,Number,Identifier,in,Number,Number,where,BooleanExpression(BooleanExpression(BooleanExpression(NumericExpression(NumericExpression(Identifier),ArithOp,NumericExpression(Number)),CompareOp,NumericExpression(Identifier)),BoolOp,BooleanExpression(NumericExpression(NumericExpression(Identifier),ArithOp,NumericExpression(Number)),CompareOp,NumericExpression(Identifier))),BoolOp,BooleanExpression(BoolOp,BooleanExpression(BooleanExpression(BooleanExpression(NumericExpression(Identifier),CompareOp,NumericExpression(Number)),BoolOp,BooleanExpression(NumericExpression(Identifier),CompareOp,NumericExpression(Number)))))))))`
        testParser(input, expected)
    })

    test("indexed predicate 2", () => {
        // Numbers aren't showing in the parse tree
        const input = `Edge(v_i, v_i) for i in [0, 20] where 20 != 20`
        const expected = `Program(PredicateApp(Identifier,ArgList(Identifier,Identifier),
            IndexedStatement(for,Identifier,in,Number,Number,where,
            BooleanExpression(NumericExpression(Number),CompareOp,NumericExpression(Number)))))`
        testParser(input, expected)
    })

    test("indexed labels 1", () => {
        // Numbers aren't showing in the parse tree
        const input = `Label x_i "abcde" for i in [0, 10]`
        const expected = `Program(Labeling(Label,Identifier,String,
            IndexedStatement(for,Identifier,in,Number,Number)))`
        testParser(input, expected)
    })

    test("indexed labels 2", () => {
        // Numbers aren't showing in the parse tree
        const input = `Label y $abc$ for j in [0, 15]`
        const expected = `Program(Labeling(Label,Identifier,TeX,
            IndexedStatement(for,Identifier,in,Number,Number)))`
        testParser(input, expected)
    })

    test("decl and decl list", () => {
        // Numbers aren't showing in the parse tree
        const input = `Set A
        Map f, g, h`
        const expected = `Program(TypeApp(Identifier,Identifier),
        TypeApp(Identifier,Identifier,Identifier,Identifier))`
        testParser(input, expected)
    })
    
    test("label decl", () => {
        // Numbers aren't showing in the parse tree
        const input = `Set A, B, C
        Label A $\\vec{A}$
        Label B $B_1$`
        const expected = `Program(TypeApp(Identifier,Identifier,Identifier,Identifier),
        Labeling(Label,Identifier,TeX),
        Labeling(Label,Identifier,TeX))`
        testParser(input, expected)
    })

    test("no label decl", () => {
        // Numbers aren't showing in the parse tree
        const input = `Set A, B, C
        NoLabel A
        NoLabel B, C`
        const expected = `Program(TypeApp(Identifier,Identifier,Identifier,Identifier),
        Labeling(NoLabel,Identifier),
        Labeling(NoLabel,Identifier,Identifier))`
        testParser(input, expected)
    })

    test("auto label decl", () => {
        // Numbers aren't showing in the parse tree
        const input = `Set A, B, C
        AutoLabel All
        AutoLabel B, C
        NoLabel B, C`
        const expected = `Program(TypeApp(Identifier,Identifier,Identifier,Identifier),
        Labeling(AutoLabel,All),
        Labeling(AutoLabel,Identifier,Identifier),
        Labeling(NoLabel,Identifier,Identifier))`
        testParser(input, expected)
    })

    test("bind and exprs", () => {
        // Numbers aren't showing in the parse tree
        const input = `Set A, B, C
        Point p1, p2
        C := Intersection(A, B)`
        const expected = `Program(TypeApp(Identifier,Identifier,Identifier,Identifier),
        TypeApp(Identifier,Identifier,Identifier),
        Fn_ConsApp(Identifier,Identifier,ArgList(Identifier,Identifier)))`
        testParser(input, expected)
    })

    test("predicates", () => {
        // Numbers aren't showing in the parse tree
        const input = `Set A, B, C
        Subset(A, B)`
        const expected = `Program(TypeApp(Identifier,Identifier,Identifier,Identifier),
        PredicateApp(Identifier,ArgList(Identifier,Identifier)))`
        testParser(input, expected)
    })

    test("numbers and strings", () => {
        // Numbers aren't showing in the parse tree
        const input = ` Set A
    Contains(A, 1)
    Contains(B, "never gonna give you up")
    Contains(C, 3.0)`
        const expected = `Program(TypeApp(Identifier,Identifier),
        PredicateApp(Identifier,ArgList(Identifier,Number)),
        PredicateApp(Identifier,ArgList(Identifier,String)),
        PredicateApp(Identifier,ArgList(Identifier,Float)))`
        testParser(input, expected)
    })


});
