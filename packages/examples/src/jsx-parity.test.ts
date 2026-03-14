// @vitest-environment jsdom

/**
 * JSX parity tests — regression guards for the @penrose/bloom JSX runtime.
 *
 * For snapshot entries: renders the JSX example and compares against a stored
 * snapshot. A snapshot mismatch means the JSX runtime changed its output.
 *
 * For trio entries (none yet): renders both a trio and its JSX port, normalises
 * floating-point numbers to 5 decimal places, and asserts the SVGs are equal.
 *
 * To update snapshots after an intentional change:
 *   npx vitest run src/jsx-parity.test.ts -u
 */
import { compile, optimize, showError, toSVG } from "@penrose/core";
import { describe, expect, test } from "vitest";
import { pairedExamples } from "./paired-registry.js";

/** Normalise floating-point numbers to 5dp to absorb trivial formatting differences */
const normalizeSVG = (svg: string): string =>
  svg.replace(/(\d+\.\d{6,})/g, (m) => parseFloat(m).toFixed(5));

const renderTrio = async (trio: {
  substance: string;
  style: string;
  domain: string;
  variation: string;
}): Promise<string> => {
  const compiled = await compile({
    substance: trio.substance,
    style: trio.style,
    domain: trio.domain,
    variation: trio.variation,
    excludeWarnings: [],
  });
  if (compiled.isErr()) throw new Error(showError(compiled.error));

  const optimized = optimize(compiled.value);
  if (optimized.isErr()) throw new Error(showError(optimized.error));

  const svg = (
    await toSVG(optimized.value, async () => undefined, "parity-test")
  ).outerHTML;
  return normalizeSVG(svg);
};

describe("jsx-parity", () => {
  for (const entry of pairedExamples) {
    test(entry.name, async () => {
      const jsxSVG = normalizeSVG(await entry.jsx());

      if (entry.kind === "snapshot") {
        // Snapshot test: guard against regressions in the JSX runtime output
        expect(jsxSVG).toMatchSnapshot();
      } else {
        // Trio comparison: JSX port must produce numerically identical SVG
        const trioSVG = await renderTrio(await entry.trio());
        expect(jsxSVG).toEqual(trioSVG);
      }
    });
  }
});
