import { examples, registry } from "@penrose/examples";
import { A } from "types/ast";
import { DomainProg } from "types/domain";
import { State } from "types/state";
import { StyProg } from "types/style";
import {
  compileTrio,
  Debugger,
  prepareState,
  RenderStatic,
  showError,
  stepUntilConvergence,
  SubProg,
} from "../index";
import { DebugProgramType, DebugStyleBlock } from "./Debugger";
import { getInconsistentDebugFunctions } from "./DebugShapeList";

/**
 * Load source code of example program
 *
 * @param uri URI string of example program
 * @returns Source of example program
 */
const exampleFromURI = (uri: string): string => {
  let x: any = examples;
  for (const part of uri.split("/")) {
    x = x[part];
  }
  return x;
};

/**
 * Renders a diagram state
 *
 * @param state State of diagram to render
 * @returns SVG string of rendered diagram
 */
const render = async (state: State): Promise<string> =>
  (await RenderStatic(state, async () => undefined)).outerHTML;

/**
 * Compiles, optimizes, and renders the diagram and returns a live debugger instance.
 *
 * @param style Style Program Source
 * @param domain Domain Program Source
 * @param substance Substance Program Source
 * @param variation Variation String
 * @returns Promise for a live Debugger instance
 */
const getLiveDebugger = async (
  style: string,
  domain: string,
  substance: string,
  variation: string
): Promise<Debugger> => {
  // Compile and Render the Diagram
  const resCompile = compileTrio({ substance, style, domain, variation });
  if (resCompile.isErr()) {
    fail(showError(resCompile.error));
  }
  const stateSample1NotOpt = await prepareState(resCompile.value);
  const resSample1Opt = stepUntilConvergence(stateSample1NotOpt);
  if (resSample1Opt.isErr()) {
    throw new Error(showError(resSample1Opt.error));
  }
  const svgSample1Opt = await render(resSample1Opt.value);

  // Get the live Debugger instance
  return Promise.resolve(Debugger.getInstance());
};

/**
 * Tests for Debugger class
 *
 * Depends on set theory examples:
 *  - venn.sty
 *  - set-theory.dsl
 *  - tree.sub
 *
 * If these programscd change, most likely these tests will need to be
 * updated as they reference specific line numbers, variables, and
 * statements within these programs.
 */
describe("Debug API", () => {
  const styleSrc = exampleFromURI(registry.styles["venn"].URI);
  const domainSrc = exampleFromURI(registry.domains["set-theory"].URI);
  const substanceSrc = exampleFromURI(registry.substances["tree"].URI);
  const variation = "determinism";
  const dummyDomainAst: DomainProg<A> = {
    tag: "DomainProg",
    nodeType: "Domain",
    statements: [],
  };
  const dummySubstanceAst: SubProg<A> = {
    tag: "SubProg",
    nodeType: "Substance",
    statements: [],
  };
  const dummyStyleAst: StyProg<A> = {
    tag: "StyProg",
    nodeType: "Style",
    blocks: [],
  };

  // Get an empty Debugger instance
  const dbgEmpty = Debugger.newInstance();

  // -------------------------- Test internal state control ------------------------- //

  test("Debug State Control", async () => {
    // Ensure debugger function registry is consistent
    expect(getInconsistentDebugFunctions()).toEqual([]);

    // Ensure: no blocks found in empty Debugger
    expect(dbgEmpty.getBlocks()).toEqual([]);

    // queryDoesStyleBlockHaveWhereClause(21) == Error (incomplete debugger cannot answer)
    expect(() => {
      dbgEmpty.queryDoesStyleBlockHaveWhereClause(21);
    }).toThrowError();

    // queryDoesStyleBlockHaveWhereClause(21) == Error (incomplete debugger cannot answer)
    dbgEmpty.setDomAst(dummyDomainAst);
    expect(() => {
      dbgEmpty.queryDoesStyleBlockHaveWhereClause(21);
    }).toThrowError();

    // queryDoesStyleBlockHaveWhereClause(21) == Error (incomplete debugger cannot answer)
    dbgEmpty.setSubAst(dummySubstanceAst);
    expect(() => {
      dbgEmpty.queryDoesStyleBlockHaveWhereClause(21);
    }).toThrowError();

    // queryDoesStyleBlockHaveWhereClause(21) == Error (incomplete debugger cannot answer)
    dbgEmpty.setStyAst(dummyStyleAst);
    expect(() => {
      dbgEmpty.queryDoesStyleBlockHaveWhereClause(21);
    }).toThrowError();

    // queryDoesStyleBlockHaveWhereClause(21) == Error (incomplete debugger cannot answer)
    dbgEmpty.setDomSrc(domainSrc);
    expect(() => {
      dbgEmpty.queryDoesStyleBlockHaveWhereClause(21);
    }).toThrowError();

    // queryDoesStyleBlockHaveWhereClause(21) == Error (incomplete debugger cannot answer)
    dbgEmpty.setSubSrc(substanceSrc);
    expect(() => {
      dbgEmpty.queryDoesStyleBlockHaveWhereClause(21);
    }).toThrowError();

    // queryDoesStyleBlockHaveWhereClause(21) == Error (incomplete debugger cannot answer)
    dbgEmpty.setStySrc(styleSrc);
    expect(() => {
      dbgEmpty.queryDoesStyleBlockHaveWhereClause(21);
    }).toThrowError();

    // getSourceRefFromAstNode(dummyStyleAst) == Error (no source ref in dummy AST)
    expect(() => {
      Debugger.getSourceRefFromAstNode(dummyStyleAst);
    }).toThrowError();

    // getSourceText() w/complete lines == (see text below)
    expect(
      Debugger.getSourceText(styleSrc, {
        origin: DebugProgramType.STYLE,
        lineStart: 1,
        lineEnd: 4,
        colStart: 0,
        colEnd: 1,
      })
    ).toEqual(["canvas {", "  width = 800", "  height = 700", "}"]);

    // getSourceText() w/endCol < startCol == (see text below)
    expect(
      Debugger.getSourceText(styleSrc, {
        origin: DebugProgramType.STYLE,
        lineStart: 1,
        lineEnd: 1,
        colStart: 0,
        colEnd: -1,
      })
    ).toEqual(["canvas {"]);

    // getSourceText() w/startCol < 1  == (Error - out of range)
    expect(() => {
      Debugger.getSourceText(styleSrc, {
        origin: DebugProgramType.STYLE,
        lineStart: 1,
        lineEnd: 1,
        colStart: -1, // Error!
        colEnd: 1,
      });
    }).toThrowError();

    // getSourceText() w/startLine < 1  == (Error - out of range)
    expect(() => {
      Debugger.getSourceText(styleSrc, {
        origin: DebugProgramType.STYLE,
        lineStart: 0, // Error!
        lineEnd: 1,
        colStart: 1,
        colEnd: 1,
      });
    }).toThrowError();

    // getSourceText() w/startLine > endLine  == (Error - out of range)
    expect(() => {
      Debugger.getSourceText(styleSrc, {
        origin: DebugProgramType.STYLE,
        lineStart: 2, // Error!
        lineEnd: 1, // Error!
        colStart: 0,
        colEnd: 1,
      });
    }).toThrowError();

    // getSourceText() w/startLine > last source line  == (Error - out of range)
    expect(() => {
      Debugger.getSourceText(styleSrc, {
        origin: DebugProgramType.STYLE,
        lineStart: 1,
        lineEnd: 999, // Error!
        colStart: 0,
        colEnd: 1,
      });
    }).toThrowError();

    // ---------------------------- Live Debugger Tests ----------------------------- //

    // Get a live Debugger instance
    const dbg = await getLiveDebugger(
      styleSrc,
      domainSrc,
      substanceSrc,
      variation
    );

    // Debugger should be in listening mode -- no user queries received yet
    expect(dbg.isListening()).toEqual<boolean>(true);
    expect(dbg.isAnswering()).toEqual<boolean>(false);

    // queryDoesStyleBlockHaveWhereClause(21) == true (able to move to answering state)
    expect(dbg.queryDoesStyleBlockHaveWhereClause(21)).toEqual<boolean>(true);

    // Debugger should be in answering mode -- user queries received
    expect(dbg.isListening()).toEqual<boolean>(false);
    expect(dbg.isAnswering()).toEqual<boolean>(true);

    // Put the debugger back into listening state by providing system input
    dbg.setSubSrc(substanceSrc);

    // Debugger should be back in listening mode -- more system input received
    expect(dbg.isListening()).toEqual<boolean>(true);
    expect(dbg.isAnswering()).toEqual<boolean>(false);

    // queryDoesStyleBlockHaveWhereClause(21) == true (able to move back to answering state)
    expect(dbg.queryDoesStyleBlockHaveWhereClause(21)).toEqual<boolean>(true);

    // Debugger should back be in answering mode again -- user queries received
    expect(dbg.isListening()).toEqual<boolean>(false);
    expect(dbg.isAnswering()).toEqual<boolean>(true);

    // Blocks found
    expect(dbg.getBlocks()).not.toEqual<DebugStyleBlock[]>([]);
  });

  // --------------- Question 1: queryDoesStyleBlockHaveWhereClause() --------------- //

  test("Debug queryDoesStyleBlockHaveWhereClause", async () => {
    // Get a live Debugger instance
    const dbg = await getLiveDebugger(
      styleSrc,
      domainSrc,
      substanceSrc,
      variation
    );

    // queryDoesStyleBlockHaveWhereClause(6) == false
    expect(dbg.queryDoesStyleBlockHaveWhereClause(6)).toEqual<boolean>(false);

    // queryDoesStyleBlockHaveWhereClause(21) == true
    expect(dbg.queryDoesStyleBlockHaveWhereClause(21)).toEqual<boolean>(true);

    // queryDoesStyleBlockHaveWhereClause(1) == Error (no style block @ line 1)
    expect(() => {
      dbg.queryDoesStyleBlockHaveWhereClause(1);
    }).toThrowError();
  });

  // -------------------- Question 2: queryDoesStyleBlockApply() -------------------- //

  test("Debug queryDoesStyleBlockApply", async () => {
    // Get a live Debugger instance
    const dbg = await getLiveDebugger(
      styleSrc,
      domainSrc,
      substanceSrc,
      variation
    );

    // queryDoesStyleBlockApply(6,{'x':'A'}) == true
    expect(dbg.queryDoesStyleBlockApply(6, { x: "A" })).toEqual<boolean>(true);

    // queryDoesStyleBlockApply(21,{'x':'A'}) == false
    expect(dbg.queryDoesStyleBlockApply(21, { x: "A" })).toEqual<boolean>(
      false
    );

    // queryDoesStyleBlockApply(21,{'y':'A'}) == true
    expect(dbg.queryDoesStyleBlockApply(21, { y: "A" })).toEqual<boolean>(true);

    // queryDoesStyleBlockApply(21,{'x':'B','y':'A'}) == true
    expect(
      dbg.queryDoesStyleBlockApply(21, { x: "B", y: "A" })
    ).toEqual<boolean>(true);

    // queryDoesStyleBlockApply(21,{'x':'B','y':'C'}) == false
    expect(
      dbg.queryDoesStyleBlockApply(21, { x: "B", y: "C" })
    ).toEqual<boolean>(false);

    // queryDoesStyleBlockApply(1,{'x':'B','y':'C'}) == Error (no style block @ line 1)
    expect(() => {
      dbg.queryDoesStyleBlockApply(1, { x: "B", y: "C" });
    }).toThrowError();
  });

  // ---------------- Question 3: queryExplainStyleBlockApplication() --------------- //

  test("Debug queryExplainStyleBlockApplication", async () => {
    // Get a live Debugger instance
    const dbg = await getLiveDebugger(
      styleSrc,
      domainSrc,
      substanceSrc,
      variation
    );

    // queryExplainStyleBlockApplication(21,{'x':'B','y':'A'}) == (see below)
    expect(
      dbg.queryExplainStyleBlockApplication(21, { x: "B", y: "A" })
    ).toEqual(
      JSON.parse(
        '[{"subst":{"x":"B","y":"A"},"relRef":{"origin":"Style","lineStart":22,"lineEnd":22,"colStart":6,"colEnd":19,"srcText":["IsSubset(x, y)"]},"reasons":[{"code":"MATCHING_SUB_STATEMENTS_FOUND","srcRef":[{"origin":"Substance","lineStart":3,"lineEnd":3,"colStart":0,"colEnd":13,"srcText":["IsSubset(B, A)"]}]}]}]'
      )
    );

    // queryExplainStyleBlockApplication(21,{'x':'B','y':'C'}) == (see below)
    expect(
      dbg.queryExplainStyleBlockApplication(21, { x: "B", y: "C" })
    ).toEqual(
      JSON.parse(
        '[{"subst":{"x":"B","y":"C"},"relRef":{"origin":"Style","lineStart":22,"lineEnd":22,"colStart":6,"colEnd":19,"srcText":["IsSubset(x, y)"]},"reasons":[{"code":"NO_MATCHING_SUB_STATEMENTS_FOUND","srcRef":[]}]}]'
      )
    );

    // queryExplainStyleBlockApplication(21,{'x':'B','y':'C'}) == Error (style variable z does not exist)
    expect(() => {
      dbg.queryExplainStyleBlockApplication(21, { z: "B" });
    }).toThrowError();

    // queryExplainStyleBlockApplication(21,{'x':'B','y':'C'}) == Error (no style block @ line 1)
    expect(() => {
      dbg.queryExplainStyleBlockApplication(1, { x: "B", y: "C" });
    }).toThrowError();

    // queryExplainStyleBlockApplication(21,{}) == Error (empty query)
    expect(() => {
      dbg.queryExplainStyleBlockApplication(21, {});
    }).toThrowError();
  });

  // ------------------------- Question 4: queryShapeFields() ----------------------- //

  test("Debug queryShapeFields", async () => {
    // Get a live Debugger instance
    const dbg = await getLiveDebugger(
      styleSrc,
      domainSrc,
      substanceSrc,
      variation
    );

    // queryShapeFields() == (see below)
    // !!! expect(dbg.queryShapeFields()).toEqual([]); // !!!
  });
});
