import { registry } from "@penrose/examples";
import { benchmarkSuite } from "jest-bench";
import { SuiteDescription } from "jest-bench/dist/suite";
import { compileTrio, readRegistry, showError } from "../index";
import { exampleFromURI } from "./util";

const compilationBenchmarks = (): SuiteDescription => {
  const tests: SuiteDescription = {};
  const trios = readRegistry(registry);
  for (const trio of trios) {
    const { name, substanceURI, domainURI, styleURI, variation } = trio;
    const [sub, sty, dsl] = [substanceURI, styleURI, domainURI].map(
      exampleFromURI
    );
    tests[`compile ${name}`] = () => {
      const res = compileTrio({
        substance: sub,
        style: sty,
        domain: dsl,
        variation,
      });

      if (res.isErr()) {
        fail(showError(res.error));
      }
    };
  }
  return tests;
};

benchmarkSuite("Energy graph compilation", {
  // setup will not run just once, it will run for each loop
  setup() {},

  // same thing with teardown
  teardown() {},

  ...compilationBenchmarks(),
});
