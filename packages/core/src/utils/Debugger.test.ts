import { examples, registry } from "@penrose/examples";
import { State } from "types/state";
import {
  compileTrio,
  Debugger,
  prepareState,
  RenderStatic,
  showError,
  stepUntilConvergence,
} from "../index";
import { DebugStyleBlock } from "./Debugger";

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

  // Get an empty Debugger instance
  const dbgEmpty = Debugger.newInstance();

  // -------------------------- Test internal state control ------------------------- //

  test("Debug State Control", async () => {
    // Ensure: no blocks found in empty Debugger
    expect(dbgEmpty.getBlocks()).toEqual([]);

    // Get a live Debugger instance
    const dbg = await getLiveDebugger(
      styleSrc,
      domainSrc,
      substanceSrc,
      variation
    );

    // queryDoesStyleBlockHaveWhereClause(21) == Error (empty debugger cannot answer)
    expect(() => {
      dbgEmpty.queryDoesStyleBlockHaveWhereClause(21);
    }).toThrowError();

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

    // queryExplainStyleBlockApplication(21,{'x':'B','y':'C'}) == Error (no style block @ line 1)
    expect(() => {
      dbg.queryExplainStyleBlockApplication(1, { x: "B", y: "C" });
    }).toThrowError();
  });
});
