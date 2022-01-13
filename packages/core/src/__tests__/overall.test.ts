import * as fs from "fs";
import * as path from "path";
import seedrandom from "seedrandom";
import {
  compileTrio,
  evalEnergy,
  evalFns,
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
        const opt = stepUntilConvergence(state);
        if (opt.isErr()) {
          fail("optimization failed");
        }
        const optimized = opt.value;

        const rendered = await RenderStatic(optimized, async (p: string) => {
          const parentDir = path.parse(path.join(EXAMPLES, styleURI)).dir;
          const actualPath = path.resolve(parentDir, p);
          const res = fs.readFileSync(actualPath, "utf-8").toString();
          return res;
        });
        fs.writeFileSync(
          path.join(OUTPUT, `${name}.svg`),
          rendered.outerHTML,
          "utf8"
        );
      } else {
        fail(showError(res.error));
      }
    });
  }
});

describe("Energy API", () => {
  test("eval overall energy - init vs. optimized", async () => {
    const twoSubsets = `Set A, B\nIsSubset(B, A)\nAutoLabel All`;
    const res = compileTrio(setDomain, twoSubsets, vennStyle);
    if (res.isOk()) {
      const stateEvaled = await prepareState(res.value);
      const stateOpt = stepUntilConvergence(stateEvaled);
      if (stateOpt.isErr()) {
        fail("optimization failed");
      }
      const stateOptimized = stateOpt.value;
      const initEng = evalEnergy(stateEvaled);
      const optedEng = evalEnergy(stateOptimized);
      expect(initEng).toBeGreaterThan(optedEng);
    } else {
      console.log(showError(res.error));
    }
  });
  test("filtered constraints", async () => {
    const twoSubsets = `Set A, B\nIsSubset(B, A)\nAutoLabel All`;
    const res = compileTrio(setDomain, twoSubsets, vennStyle);
    if (res.isOk()) {
      // NOTE: delibrately not cache the overall objective and re-generate for original and filtered states
      const state = res.value;
      const smallerThanFns = state.constrFns.filter(
        (c) => c.fname === "smallerThan"
      );
      const stateFiltered = { ...state, constrFns: smallerThanFns };
      expect(evalEnergy(state)).toBeGreaterThan(evalEnergy(stateFiltered));
    } else {
      console.log(showError(res.error));
    }
  });
});

describe("Cross-instance energy eval", () => {
  test("correct - subsets", async () => {
    const twosets = `Set A, B\nAutoLabel All`;
    const twoSubsets = `Set A, B\nIsSubset(B, A)\nAutoLabel All`;
    // compile and optimize both states
    const state1 = compileTrio(setDomain, twosets, vennStyle);
    const state2 = compileTrio(setDomain, twoSubsets, vennStyle);
    if (state1.isOk() && state2.isOk()) {
      const state1Done = stepUntilConvergence(await prepareState(state1.value));
      const state2Done = stepUntilConvergence(await prepareState(state2.value));
      if (state1Done.isOk() && state2Done.isOk()) {
        const crossState21 = {
          ...state2Done.value,
          constrFns: state1Done.value.constrFns,
          objFns: state1Done.value.objFns,
        };
        expect(evalEnergy(await prepareState(crossState21))).toBeCloseTo(0);
        const crossState12 = {
          ...state1Done.value,
          constrFns: state2Done.value.constrFns,
          objFns: state2Done.value.objFns,
        };
        expect(evalEnergy(await prepareState(crossState12))).toBeGreaterThan(0);
      } else {
        fail("optimization failed");
      }
    } else {
      fail("compilation failed");
    }
  });
});

describe("Run individual functions", () => {
  // TODO: Test evalFns vs overall objective? Also, test individual functions more thoroughly
  const EPS = 1e-3; // Minimized objectives should be close to 0

  test("Check each individual function is minimized/satisfied", async () => {
    const twoSubsets = `Set A, B\nIsSubset(B, A)\nAutoLabel All`;
    const res = compileTrio(setDomain, twoSubsets, vennStyle);

    if (res.isOk()) {
      const stateEvaled = await prepareState(res.value);
      const stateOpt = stepUntilConvergence(stateEvaled);
      if (stateOpt.isErr()) {
        fail("optimization failed");
      }
      const stateOptimizedValue = stateOpt.value;

      // console.log("# objectives", stateEvaled.objFns.length);
      // console.log("# constraints", stateEvaled.constrFns.length);

      // Test objectives
      const initEngsObj = evalFns(stateEvaled.objFns, stateEvaled);
      const optedEngsObj = evalFns(stateEvaled.objFns, stateOptimizedValue);

      for (let i = 0; i < initEngsObj.length; i++) {
        // console.log("obj energies", initEngsObj[i], optedEngsObj[i]);
        expect(initEngsObj[i].f).toBeGreaterThanOrEqual(optedEngsObj[i].f);
        expect(optedEngsObj[i].f).toBeLessThanOrEqual(EPS);
      }

      // Test constraints
      const initEngsConstr = evalFns(stateEvaled.constrFns, stateEvaled);
      const optedEngsConstr = evalFns(
        stateEvaled.constrFns,
        stateOptimizedValue
      );

      for (let i = 0; i < initEngsConstr.length; i++) {
        // console.log("constr energies", initEngsConstr[i], optedEngsConstr[i]);
        if (initEngsConstr[i].f < 0 && optedEngsConstr[i].f < 0) {
          // If it was already satisfied and stays satisfied, the magnitude of the constraint doesn't matter (i.e. both are negative)
          expect(true).toEqual(true);
        } else {
          expect(initEngsConstr[i].f).toBeGreaterThanOrEqual(
            optedEngsConstr[i].f
          );
          // Check constraint satisfaction (< 0)
          expect(optedEngsConstr[i].f).toBeLessThanOrEqual(0);
        }
      }
    } else {
      console.log(showError(res.error));
    }
  });
});
