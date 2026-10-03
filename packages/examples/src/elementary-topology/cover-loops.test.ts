// @vitest-environment jsdom
import { describe, expect, test, vi } from "vitest";
import { diagram } from "../../../bloom/src/core/program.js";
import { canvas } from "../../../bloom/src/core/utils.js";
import {
  circularBasedLoop,
  piecewisePlaneLoopData,
  piecewisePlaneLoopValue,
  planeCurveSegmentValue,
  planeLoopContractionValue,
  planeLoopDiskBound,
} from "../../../bloom/src/domains/plane-curves.js";
import { pointSetTopology as topology } from "../../../bloom/src/domains/point-set-topology.js";
import { planeLoopPathData } from "../../../bloom/src/styles/plane-curves.js";
import {
  buildCircularLoopPencilFigure,
  buildCoverLoopFigure,
  circularLoopPencil,
  coverFanCurveData,
  coverLoopFan,
} from "./cover-loops.js";

describe("native plane loop families", () => {
  test("the cover reconstruction contains eight continuous closed loops with the same base point", () => {
    const loops = coverFanCurveData(),
      base = piecewisePlaneLoopValue(loops[0], 0);
    expect(loops).toHaveLength(8);
    for (const loop of loops) {
      expect(piecewisePlaneLoopValue(loop, 0)).toEqual(base);
      expect(piecewisePlaneLoopValue(loop, 1)).toEqual(base);
      for (const [i, s] of loop.segments.entries()) {
        const next = loop.segments[(i + 1) % loop.segments.length];
        const a = planeCurveSegmentValue(s, 1),
          b = planeCurveSegmentValue(next, 0);
        expect(Math.hypot(a[0] - b[0], a[1] - b[1])).toBeLessThan(1e-8);
      }
    }
  });
  test("representative cover curves stay within the asserted disk, including exact boundary arcs", () => {
    for (const loop of coverFanCurveData()) {
      for (let i = 0; i <= 2048; i++) {
        const p = piecewisePlaneLoopValue(loop, i / 2048);
        expect(Math.hypot(...p)).toBeLessThanOrEqual(1 + 1e-10);
      }
    }
  });
  test("outward-rounded Bernstein bounds certify the whole cover curves despite outside controls", () => {
    const loops = coverFanCurveData();
    expect(
      loops.some((l) =>
        l.segments.some(
          (s) =>
            s.kind === "cubic" && s.controls.some((p) => Math.hypot(...p) > 1),
        ),
      ),
    ).toBe(true);
    for (const loop of loops) {
      const certificate = planeLoopDiskBound(loop, [0, 0], 1);
      expect(certificate.contained).toBe(true);
      expect(certificate.tolerance).toBe(1e-12);
      expect(certificate.squaredRadiusUpperBound).toBeLessThanOrEqual(
        (1 + 1e-12) ** 2,
      );
      expect(certificate.subdivisions).toBeLessThan(8192);
    }
  });
  test("whole-curve bounds reject an excursion missed by coarse sampling and never accept unresolved bounds", () => {
    const excursion = piecewisePlaneLoopData([
      {
        kind: "cubic",
        controls: [
          [0, 0],
          [4, 0],
          [-4, 0],
          [0, 0],
        ],
      },
    ]);
    for (const t of [0, 0.5, 1])
      expect(Math.hypot(...piecewisePlaneLoopValue(excursion, t))).toBe(0);
    const critical = (3 - Math.sqrt(3)) / 6;
    expect(
      Math.hypot(...piecewisePlaneLoopValue(excursion, critical)),
    ).toBeGreaterThan(1.15);
    const certificate = planeLoopDiskBound(excursion, [0, 0], 1);
    expect(certificate.contained).toBe(false);
    expect(certificate.squaredRadiusUpperBound).toBeGreaterThan(1);
    const constant = piecewisePlaneLoopData([
      {
        kind: "cubic",
        controls: [
          [1, 0],
          [1, 0],
          [1, 0],
          [1, 0],
        ],
      },
    ]);
    const unresolved = planeLoopDiskBound(constant, [0, 0], 1, {
      tolerance: 0,
      maxSubdivisions: 8,
    });
    expect(unresolved.contained).toBe(false);
    expect(unresolved.subdivisions).toBe(8);
    expect(planeLoopDiskBound(constant, [0, 0], 1).contained).toBe(true);
  });
  test("linear contractions fix the common base, close every slice and end at the constant loop", () => {
    for (const loop of coverFanCurveData()) {
      const base = piecewisePlaneLoopValue(loop, 0);
      for (const h of [0, 0.25, 0.75, 1]) {
        expect(planeLoopContractionValue(loop, 0, h)).toEqual(base);
        expect(planeLoopContractionValue(loop, 1, h)).toEqual(base);
        for (let i = 0; i <= 32; i++) {
          const p = planeLoopContractionValue(loop, i / 32, h);
          expect(Math.hypot(...p)).toBeLessThanOrEqual(1 + 1e-10);
          if (h === 0) expect(p).toEqual(piecewisePlaneLoopValue(loop, i / 32));
          if (h === 1) expect(p).toEqual(base);
        }
      }
    }
  });
  test("loop data is copied and deeply immutable, and rejects disconnected or nonfinite pieces", () => {
    const controls: [
        [number, number],
        [number, number],
        [number, number],
        [number, number],
      ] = [
        [0, 0],
        [1, 0],
        [0, 1],
        [0, 0],
      ],
      loop = piecewisePlaneLoopData([{ kind: "cubic", controls }]);
    controls[1][0] = 7;
    expect(
      loop.segments[0].kind === "cubic" && loop.segments[0].controls[1][0],
    ).toBe(1);
    expect(Object.isFrozen(loop.segments[0])).toBe(true);
    expect(
      Object.isFrozen(
        loop.segments[0].kind === "cubic" && loop.segments[0].controls[1],
      ),
    ).toBe(true);
    expect(() =>
      piecewisePlaneLoopData([
        {
          kind: "cubic",
          controls: [
            [0, 0],
            [1, 0],
            [0, 1],
            [1, 1],
          ],
        },
      ]),
    ).toThrow("close");
    expect(() => circularBasedLoop([0, 0], [0, 0])).toThrow("radius");
    expect(() => circularBasedLoop([Number.NaN, 0], [1, 0])).toThrow("finite");
  });
  test("exact circular loops have constant radius and split full native circles into two correct SVG arcs", () => {
    const center = [0.2, -0.1] as const,
      loop = circularBasedLoop(center, [0.2, 0.4]);
    for (let i = 0; i <= 64; i++) {
      const p = piecewisePlaneLoopValue(loop, i / 64);
      expect(Math.hypot(p[0] - center[0], p[1] - center[1])).toBeCloseTo(
        0.5,
        12,
      );
    }
    const data = planeLoopPathData(loop, 100, [7, 9]),
      arcs = data.filter((c) => c.cmd === "A");
    expect(arcs).toHaveLength(2);
    expect(arcs[0].contents[0]).toEqual({
      tag: "ValueV",
      contents: [50, 50, 0, 0, 0],
    });
    expect(data[0].contents[0].tag).toBe("CoordV");
    expect(data[0].contents[0].contents[0]).toBeCloseTo(27, 12);
    expect(data[0].contents[0].contents[1]).toBeCloseTo(49, 12);
    expect(data.at(-1)?.cmd).toBe("Z");
  });
  test("both shape-free programs encode genuine based loops and nullhomotopy in the disk", async () => {
    for (const [program, count] of [
      [coverLoopFan(), 8],
      [circularLoopPencil(), 4],
    ] as const) {
      const d = await diagram({
        sub: program,
        canvas: canvas(100, 100),
        sty: topology.style((ctx) => {
          expect(ctx.entities(topology.PiecewisePlaneLoop)).toHaveLength(count);
          expect(ctx.facts(topology.LoopInPlaneFamily)).toHaveLength(count);
          expect(ctx.facts(topology.LoopBasedAt)).toHaveLength(2 * count);
          expect(ctx.facts(topology.NullHomotopic)).toHaveLength(count);
          expect(ctx.facts(topology.HomotopyBetween)).toHaveLength(count);
          // Book convention is H(-,1)=f and H(-,0)=g, whereas our contraction ends at constant.
          const constants = ctx.entities(topology.ConstantLoop),
            loops = ctx.entities(topology.PiecewisePlaneLoop);
          for (const [, f, g] of ctx.facts(topology.HomotopyBetween)) {
            expect(constants.some((k) => k === f)).toBe(true);
            const loop = loops.find((l) => l === g)!;
            expect(loop).toBeTruthy();
            expect(planeLoopContractionValue(loop, 0.25, 0)).toEqual(
              piecewisePlaneLoopValue(loop, 0.25),
            );
            expect(planeLoopContractionValue(loop, 0.25, 1)).toEqual(
              piecewisePlaneLoopValue(loop, 0),
            );
          }
        }),
      });
      d.discard();
    }
  });
  test("both families render through native optimized paths, with no image or text wrappers", async () => {
    const diagrams = await Promise.all([
      buildCoverLoopFigure(),
      buildCircularLoopPencilFigure({
        variation: "another-circle-pencil",
        interactive: true,
      }),
    ]);
    try {
      for (const [i, d] of diagrams.entries()) {
        while (await d.optimizationStep()) {
          // Optimize to convergence before inspecting the native geometry.
        }
        const { svg } = await d.render(),
          paths = [...svg.querySelectorAll("path")];
        expect(paths).toHaveLength(i === 0 ? 8 : 4);
        expect(svg.querySelectorAll("image,text")).toHaveLength(0);
        expect(svg.querySelectorAll("circle")).toHaveLength(1);
        for (const p of paths) {
          expect(p.getAttribute("d")).toMatch(/Z\s*$/);
          expect(p.getAttribute("d")).not.toMatch(/NaN|Infinity/);
        }
        const moves = paths.map((p) =>
          p
            .getAttribute("d")!
            .match(/^M\s+([^A-Z]+)/)![1]
            .trim(),
        );
        expect(new Set(moves).size).toBe(1);
      }
    } finally {
      diagrams.forEach((d) => d.discard());
    }
  });
  test("whole-family native dragging preserves every curve and circular boundary incidence", async () => {
    const d = await buildCoverLoopFigure({ interactive: { jitter: 0 } });
    try {
      while (await d.optimizationStep()) {
        // Optimize to convergence before inspecting the native geometry.
      }
      const before = (await d.render()).svg,
        nativeBefore = [...before.querySelectorAll("path")].map(
          (p) => p.getAttribute("d")!,
        ),
        handle = "plane-loop-family.boundary";
      expect(before.querySelectorAll('[data-bloom-drag="true"]')).toHaveLength(
        1,
      );
      expect(d.getDraggingConstraints().size).toBe(1);
      d.beginDrag(handle);
      d.translate(handle, 3, -2);
      d.endDrag(handle);
      while (await d.optimizationStep()) {
        // Optimize to convergence before inspecting the native geometry.
      }
      const after = (await d.render()).svg,
        circle = after.querySelector("circle")!;
      expect(Number(circle.getAttribute("cx"))).toBeCloseTo(365, 6);
      expect(Number(circle.getAttribute("cy"))).toBeCloseTo(364, 6);
      expect(circle.getAttribute("r")).toBe("353");
      const commands = (s: string) =>
        [...s.matchAll(/([MCAZ])([^MCAZ]*)/g)].map(([, cmd, ns]) => ({
          cmd,
          values: ns.trim() ? ns.trim().split(/\s+/).map(Number) : [],
        }));
      for (const [i, path] of [...after.querySelectorAll("path")].entries()) {
        const a = commands(nativeBefore[i]),
          b = commands(path.getAttribute("d")!);
        expect(b.map((s) => s.cmd)).toEqual(a.map((s) => s.cmd));
        for (const [j, c] of b.entries()) {
          const start = c.cmd === "A" ? 5 : 0;
          if (start)
            expect(c.values.slice(0, start)).toEqual(
              a[j].values.slice(0, start),
            );
          c.values
            .slice(start)
            .forEach((v, k) =>
              expect(v - a[j].values[start + k]).toBeCloseTo(k % 2 ? 2 : 3, 6),
            );
        }
      }
    } finally {
      d.discard();
    }
  });
  test("sampled family translations repeat for a seed, differ across seeds, and stay bounded", async () => {
    const builds = await Promise.all(
      ["cover-seed-a", "cover-seed-a", "cover-seed-b"].map((variation) =>
        buildCoverLoopFigure({ variation, interactive: true }),
      ),
    );
    try {
      const centers: number[] = [];
      for (const d of builds) {
        while (await d.optimizationStep()) {
          // Optimize to convergence before inspecting the native geometry.
        }
        const x = d.getInput("plane-loop-family.boundary.layout.x");
        centers.push(x);
        expect(Math.abs(x)).toBeLessThanOrEqual(3 + 1e-8);
        expect(
          Math.abs(d.getInput("plane-loop-family.boundary.layout.y")),
        ).toBeLessThanOrEqual(3 + 1e-8);
      }
      expect(centers[0]).toBeCloseTo(centers[1], 8);
      expect(centers[0]).not.toBeCloseTo(centers[2], 4);
    } finally {
      builds.forEach((d) => d.discard());
    }
  });
  test("the native cover handle responds to keyboard movement in the live SVG", async () => {
    vi.stubGlobal("requestAnimationFrame", (cb: () => void) =>
      setTimeout(cb, 0),
    );
    vi.stubGlobal("cancelAnimationFrame", (id: ReturnType<typeof setTimeout>) =>
      clearTimeout(id),
    );
    const d = await buildCoverLoopFigure({ interactive: { jitter: 0 } }),
      host = d.getInteractiveElement();
    const waitFor = async (check: () => boolean) => {
      for (let i = 0; i < 100; i++) {
        if (check()) return;
        await new Promise((resolve) => setTimeout(resolve, 5));
      }
      throw new Error("Native cover frame did not update");
    };
    try {
      document.body.appendChild(host);
      await waitFor(() => !!host.querySelector('[data-bloom-drag="true"]'));
      const handle = host.querySelector('[data-bloom-drag="true"]')!,
        before = handle.getAttribute("cx");
      handle.dispatchEvent(
        new KeyboardEvent("keydown", { key: "ArrowRight", bubbles: true }),
      );
      await waitFor(() => handle.getAttribute("cx") !== before);
      expect(d.getInput("plane-loop-family.boundary.layout.x")).toBeCloseTo(
        2,
        5,
      );
      expect(handle.getAttribute("tabindex")).toBe("0");
      expect(host.querySelectorAll("path")).toHaveLength(8);
    } finally {
      d.discard();
      host.remove();
      vi.unstubAllGlobals();
    }
  });
});
