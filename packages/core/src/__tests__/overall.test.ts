import * as fs from "fs";
import * as path from "path";
import seedrandom from "seedrandom";
import {
  compileTrio,
  evalEnergy,
  prepareState,
  readRegistry,
  RenderStatic,
  showError,
  stepUntilConvergence,
} from "../index";

const OUTPUT = "/tmp/diagrams";
const EXAMPLES = "../../examples";
const registryPath = path.join(EXAMPLES, "registry.json");
const saveDiagrams = true;
const registry = JSON.parse(fs.readFileSync(registryPath).toString());
const vennStyle = fs
  .readFileSync(path.join(EXAMPLES, registry.styles["venn"].URI))
  .toString();
const setDomain = fs
  .readFileSync(path.join(EXAMPLES, registry.domains["set-theory"].URI))
  .toString();

describe("End-to-end testing of existing diagrams", () => {
  const registry = JSON.parse(fs.readFileSync(registryPath).toString());
  const trios = readRegistry(registry);
  for (const trio of trios) {
    const { name, substanceURI, domainURI, styleURI } = trio;
    const [sub, sty, dsl] = [substanceURI, styleURI, domainURI].map((uri) =>
      fs.readFileSync(path.join(EXAMPLES, uri)).toString()
    );
    test(name, async () => {
      seedrandom("secret-seed", { global: true }); // HACK: constant seed for pseudorandomness
      if (saveDiagrams && !fs.existsSync(OUTPUT)) {
        fs.mkdirSync(OUTPUT);
      }
      const res = compileTrio(dsl, sub, sty);
      if (res.isOk()) {
        const state = await prepareState(res.value);
        const optimized = stepUntilConvergence(state);
        const rendered = RenderStatic(optimized);
        fs.writeFileSync(
          path.join(OUTPUT, `${name}.svg`),
          rendered.outerHTML,
          "utf8"
        );
      } else {
        fail(res.error);
      }
    });
  }
});

describe("Energy API", () => {
  test("eval overall energy - init", async () => {
    const twoSubsets = `Set A, B\nIsSubset(B, A)\nAutoLabel All`;
    const res = compileTrio(setDomain, twoSubsets, vennStyle);
    if (res.isOk()) {
      const stateEvaled = await prepareState(res.value);
      const stateOptimized = stepUntilConvergence(stateEvaled);
      console.log(evalEnergy(stateEvaled));
      console.log(evalEnergy(stateOptimized));
    } else {
      console.log(showError(res.error));
    }
  });
});

// describe("Energy API", () => {
//   test("filter constraints", () => {
//     const twoSubsets = `Set A, B\nIsSubset(B, A)\nAutoLabel All`;
//     const res = compileTrio(setDomain, twoSubsets, vennStyle);
//     if (res.isOk()) {
//       const state = res.value;
//       const containFns = state.constrFns.filter((c) => c.fname === "contains");
//       const xs = state.varyingValues;

//       console.log(e(xs));

//       console.log(containFns);
//     } else {
//       console.log(showError(res.error));
//     }
//   });
// });

describe("Cross-instance energy eval", () => {
  test("correct - subsets", async () => {
    const twosets = `Set A, B\nAutoLabel All`;
    const twoSubsets = `Set A, B\nIsSubset(B, A)\nAutoLabel All`;
    const state1 = compileTrio(setDomain, twosets, vennStyle);
    const state2 = compileTrio(setDomain, twoSubsets, vennStyle);
    // if (res.isOk()) {
    //   const state = await prepareState(res.value);
    //   const optimized = stepUntilConvergence(state);
    //   const rendered = RenderStatic(optimized);
    //   fs.writeFileSync(
    //     path.join(OUTPUT, `${name}.svg`),
    //     rendered.outerHTML,
    //     "utf8"
    //   );
    // } else {
    //   fail(res.error);
    // }
  });
});
