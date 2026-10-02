// @vitest-environment jsdom
import {
  metricSpaces,
  powerSequencePointwiseLimit,
  powerSequenceTerm,
  powerSequenceUniformDistance,
} from "@penrose/bloom";
import { describe, expect, test } from "vitest";
import {
  buildPowerFunctionSequenceFigure,
  buildPowerSequenceLimitFigure,
  powerFunctionSequence,
} from "./function-sequences.js";

describe("power sequences and the uniform metric", () => {
  test("preserves the discontinuous endpoint limit and nonattained supremum", () => {
    expect(powerSequencePointwiseLimit(0.75)).toBe(0);
    expect(powerSequencePointwiseLimit(1)).toBe(1);
    expect(powerSequenceTerm(100, 0.75)).toBeLessThan(1e-12);
    expect(powerSequenceTerm(100, 1)).toBe(1);
    for (const n of [1, 2, 3, 100]) {
      expect(powerSequenceUniformDistance(n)).toBe(1);
      const nearEndpoint = Math.exp(Math.log(0.9) / n);
      expect(powerSequenceTerm(n, nearEndpoint)).toBeCloseTo(0.9);
      expect(powerSequencePointwiseLimit(nearEndpoint)).toBe(0);
    }
    expect(() => powerSequenceTerm(0, 0.5)).toThrow("positive integer");
    expect(() => powerSequencePointwiseLimit(1.1)).toThrow("domain [0,1]");
  });

  test("asserts pointwise convergence and failed uniform convergence in shape-free substances", () => {
    const sub = powerFunctionSequence({ collar: true });
    expect(
      sub.propositions.filter(
        (p) => p.predicate === metricSpaces.PointwiseConvergesTo,
      ),
    ).toHaveLength(1);
    expect(
      sub.propositions.filter(
        (p) => p.predicate === metricSpaces.FailsToConvergeUniformlyTo,
      ),
    ).toHaveLength(1);
    expect(sub.entities.every((entity) => !("shapeType" in entity))).toBe(true);
  });

  test("renders the exact displayed powers with common endpoints", async () => {
    const drawing = await buildPowerFunctionSequenceFigure();
    try {
      const svg = (await drawing.render()).svg;
      const graphs = svg.querySelectorAll("polyline");
      expect(graphs).toHaveLength(6);
      for (const graph of graphs) {
        const coordinates = graph
          .getAttribute("points")!
          .trim()
          .split(/[\s,]+/)
          .map(Number);
        expect(coordinates.slice(0, 2)).toEqual([48, 247]);
        expect(coordinates.slice(-2)).toEqual([252, 43]);
      }
      const square = svg.querySelector("rect")!;
      expect(Number(square.getAttribute("width"))).toBe(204);
      expect(Number(square.getAttribute("height"))).toBe(204);
    } finally {
      drawing.discard();
    }
  });

  test("renders the lower 1/3 collar and separate endpoint fiber", async () => {
    const drawing = await buildPowerSequenceLimitFigure();
    try {
      const svg = (await drawing.render()).svg;
      const band = Array.from(svg.querySelectorAll("rect")).find(
        (rect) =>
          rect.querySelector("title")?.textContent === "limit.lower-collar",
      )!;
      expect(Number(band.getAttribute("height"))).toBeCloseTo(68);
      expect(svg.querySelectorAll("polyline")).toHaveLength(1);
      const endpointValues = [...svg.querySelectorAll("circle")]
        .map((dot) => Number(dot.getAttribute("cy")))
        .sort((a, b) => a - b);
      expect(endpointValues).toHaveLength(4);
      [43, 111, 179, 247].forEach((value, index) =>
        expect(endpointValues[index]).toBeCloseTo(value),
      );
      expect(svg.querySelector("line[marker-end]")).not.toBeNull();
    } finally {
      drawing.discard();
    }
  });
});
