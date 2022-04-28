import { examples, registry } from "@penrose/examples";
import {
  compileTrio,
  evalEnergy,
  evalFns,
  prepareState,
  RenderStatic,
  resample,
  showError,
  stepUntilConvergence,
  variationSeeds,
} from "../index";
import { State } from "../types/state";

const exampleFromURI = (uri: string): string => {
  let x: any = examples;
  for (const part of uri.split("/")) {
    x = x[part];
  }
  return x;
};

const vennStyle = exampleFromURI(registry.styles["venn"].URI);
const setDomain = exampleFromURI(registry.domains["set-theory"].URI);

describe("Determinism", () => {
  const render = async (state: State): Promise<string> =>
    (await RenderStatic(state, async () => undefined)).outerHTML;

  const substance = "Set A, B\nIsSubset(B, A)\nAutoLabel All";
  const style = vennStyle;
  const domain = setDomain;
  const variation = "determinism";

  test("with initial optimization", async () => {
    const resCompile = compileTrio({ substance, style, domain, variation });
    if (resCompile.isErr()) {
      fail(showError(resCompile.error));
    }
    const stateSample1NotOpt = await prepareState(resCompile.value);
    const svgSample1NotOpt = await render(stateSample1NotOpt);

    const resSample1Opt = stepUntilConvergence(stateSample1NotOpt);
    if (resSample1Opt.isErr()) {
      fail(showError(resSample1Opt.error));
    }
    const stateSample1Opt = resSample1Opt.value;
    const svgSample1Opt = await render(stateSample1Opt);

    const stateSample2NotOpt = resample(stateSample1Opt);
    const svgSample2NotOpt = await render(stateSample2NotOpt);

    const resSample2Opt = stepUntilConvergence(stateSample2NotOpt);
    if (resSample2Opt.isErr()) {
      fail(showError(resSample2Opt.error));
    }
    const stateSample2Opt = resSample2Opt.value;
    const svgSample2Opt = await render(stateSample2Opt);

    const stateSample3NotOpt = resample(stateSample2Opt);
    const svgSample3NotOpt = await render(stateSample3NotOpt);

    const resSample3Opt = stepUntilConvergence(stateSample3NotOpt);
    if (resSample3Opt.isErr()) {
      fail(showError(resSample3Opt.error));
    }
    const stateSample3Opt = resSample3Opt.value;
    const svgSample3Opt = await render(stateSample3Opt);

    expect(svgSample1NotOpt).not.toBe(svgSample1Opt); // optimization does something
    expect(svgSample1NotOpt).not.toBe(svgSample2NotOpt); // resampling is different from initial sampling
    expect(svgSample1NotOpt).not.toBe(svgSample2Opt); // optimizing resample doesn't get us back

    expect(svgSample1Opt).not.toBe(svgSample2NotOpt); // resampling is different from optimization
    expect(svgSample1Opt).not.toBe(svgSample2Opt); // different starts, different ends

    expect(svgSample2NotOpt).not.toBe(svgSample2Opt); // optimization does something
    expect(svgSample2NotOpt).toBe(svgSample3NotOpt); // resampling is idempotent

    expect(svgSample2Opt).toBe(svgSample3Opt); // optimization is deterministic
  });

  test("without initial optimization", async () => {
    const resCompile = compileTrio({ substance, style, domain, variation });
    if (resCompile.isErr()) {
      fail(showError(resCompile.error));
    }

    const state1NotOpt = resample(await prepareState(resCompile.value));
    const svg1NotOpt = await render(state1NotOpt);

    const resOptimize1 = stepUntilConvergence(state1NotOpt);
    if (resOptimize1.isErr()) {
      fail(showError(resOptimize1.error));
    }
    const state1Opt = resOptimize1.value;
    const svg1Opt = await render(state1Opt);

    state1Opt.seeds = variationSeeds(variation).seeds;

    const state2NotOpt = resample(state1Opt);
    const svg2NotOpt = await render(state2NotOpt);

    const resOptimize2 = stepUntilConvergence(state2NotOpt);
    if (resOptimize2.isErr()) {
      fail(showError(resOptimize2.error));
    }
    const state2Opt = resOptimize2.value;
    const svg2Opt = await render(state2Opt);

    expect(svg1NotOpt).not.toBe(svg1Opt);
    expect(svg1NotOpt).toBe(svg2NotOpt);
    expect(svg1Opt).toBe(svg2Opt);
  });
});

describe("Energy API", () => {
  test("eval overall energy - init vs. optimized", async () => {
    const twoSubsets = `Set A, B\nIsSubset(B, A)\nAutoLabel All`;
    const res = compileTrio({
      substance: twoSubsets,
      style: vennStyle,
      domain: setDomain,
      variation: "energy overall",
    });
    if (res.isOk()) {
      // resample because initial sampling did not use the special sampling seed
      const stateEvaled = resample(await prepareState(res.value));
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
    const res = compileTrio({
      substance: twoSubsets,
      style: vennStyle,
      domain: setDomain,
      variation: "energy filtered",
    });
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
    const state1 = compileTrio({
      substance: twosets,
      style: vennStyle,
      domain: setDomain,
      variation: "cross-instance state1",
    });
    const state2 = compileTrio({
      substance: twoSubsets,
      style: vennStyle,
      domain: setDomain,
      variation: "cross-instance state2",
    });
    if (state1.isOk() && state2.isOk()) {
      // resample because initial sampling did not use the special sampling seed
      const state1Done = stepUntilConvergence(
        resample(await prepareState(state1.value))
      );
      const state2Done = stepUntilConvergence(
        resample(await prepareState(state2.value))
      );
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
    const res = compileTrio({
      substance: twoSubsets,
      style: vennStyle,
      domain: setDomain,
      variation: "individual functions",
    });

    if (res.isOk()) {
      // resample because initial sampling did not use the special sampling seed
      const stateEvaled = resample(await prepareState(res.value));
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
