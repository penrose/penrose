import * as path from "path";
import * as fs from "fs";
import {
  compileTrio,
  prepareState,
  initializeMat,
  RenderStatic,
  stepUntilConvergence,
} from "../index";

describe("Render", () => {
  const triple = [
    "../../examples/set-theory-domain/setTheory.dsl",
    "../../examples/set-theory-domain/twosets-simple.sub",
    "../../examples/set-theory-domain/venn.sty",
  ].map((p) => fs.readFileSync(p).toString()) as [string, string, string];
  test("Diagram", async () => {
    const res = compileTrio(...triple);
    if (res.isOk()) {
      const state = await prepareState(res.value);
      const optimized = stepUntilConvergence(state);
      // const rendered = RenderStatic(optimized);
      // console.log(rendered.outerHTML);
      // fs.writeFileSync("/tmp/output.svg", rendered.outerHTML, "utf8");
    } else {
      fail(res.error);
    }
  });
});
