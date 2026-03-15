// @vitest-environment jsdom

/**
 * Smoke tests: verify that bloom JSX examples produce non-empty SVG strings.
 * These run in jsdom so the Bloom renderer has a DOM available.
 */
import { describe, expect, test } from "vitest";

describe("bloom JSX examples smoke test", () => {
  test("simple-circles renders an SVG", async () => {
    const { default: run } = await import("./bloom/gallery/simple-circles.js");
    const svg = await run();
    expect(svg).toContain("<svg");
    expect(svg).toContain("circle");
  });

  test("custom-components renders an SVG", async () => {
    const { default: run } = await import("./bloom/gallery/custom-components.js");
    const svg = await run();
    expect(svg).toContain("<svg");
    expect(svg).toContain("circle");
  });
});
