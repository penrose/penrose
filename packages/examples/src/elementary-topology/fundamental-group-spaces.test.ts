// @vitest-environment jsdom
import {
  basepointConjugateValue,
  canvas,
  diagram,
  finiteGraphCycleRank,
  sphereToTangent,
  stereographicLoopContraction,
  tangentToSphere,
  textbookGlyphOutline,
  pointSetTopology as topology,
  torusFactorPoint,
} from "@penrose/bloom";
import { describe, expect, test } from "vitest";
import {
  buildBasepointChangeFigure,
  buildContractibleEquivalenceFigure,
  buildPlanarHomotopyExercisesFigure,
  buildSphereLoopFigure,
  buildTorusGeneratorsFigure,
  contractibleSingletonEquivalence,
  planarHomotopyExerciseSpaces,
  puncturedSphereLoop,
  torusFundamentalGenerators,
  transportedLoopBasepoint,
} from "./fundamental-group-spaces.js";
describe("based sphere and torus constructions", () => {
  test("stereographic projection is invertible and contraction fixes the basepoint while avoiding the pole", () => {
    const pole = [0, 0, 1] as const,
      base = tangentToSphere([-1, -1, -1], pole);
    for (let i = 0; i <= 100; i++) {
      const theta = (2 * Math.PI * i) / 100,
        p = tangentToSphere(
          [-1 + Math.cos(theta) - 1, -1 + Math.sin(theta), -1],
          pole,
        ),
        plane = sphereToTangent(p, pole);
      expect(plane[2]).toBeCloseTo(-1, 12);
      tangentToSphere(plane, pole).forEach((v, j) =>
        expect(v).toBeCloseTo(p[j], 12),
      );
      for (const t of [0, 0.25, 0.5, 0.75, 1]) {
        const q = stereographicLoopContraction(p, base, pole, t);
        expect(Math.hypot(...q)).toBeCloseTo(1, 12);
        expect(q[2]).toBeLessThan(1);
        if (t === 0) q.forEach((v, j) => expect(v).toBeCloseTo(p[j], 12));
        if (t === 1) q.forEach((v, j) => expect(v).toBeCloseTo(base[j], 12));
        stereographicLoopContraction(base, base, pole, t).forEach((v, j) =>
          expect(v).toBeCloseTo(base[j], 12),
        );
      }
    }
    expect(() => sphereToTangent(pole, pole)).toThrow();
    expect(() => tangentToSphere([1, 1, 0], pole)).toThrow();
    expect(() => stereographicLoopContraction(base, base, pole, 1.1)).toThrow();
  });
  test("the punctured sphere contracts but the full sphere does not", async () => {
    const d = await diagram({
      sub: puncturedSphereLoop(),
      canvas: canvas(226, 238),
      sty: topology.style((ctx) => {
        const sphere = ctx.entities(topology.Sphere)[0],
          [, loop, , pole, base] = ctx.facts(
            topology.SphereLoopContractionOf,
          )[0],
          tau = ctx
            .facts(topology.TopologyOn)
            .find(([, s]) => s === sphere)![0];
        expect(ctx.test(topology.SimplyConnected, tau)).toBe(true);
        expect(ctx.test(topology.NotContractible, tau)).toBe(true);
        expect(ctx.test(topology.Contractible, tau)).toBe(false);
        expect(ctx.test(topology.SphereLoopAvoids, loop, pole, sphere)).toBe(
          true,
        );
        expect(ctx.test(topology.NullHomotopic, loop, tau, base)).toBe(true);
        expect(ctx.facts(topology.Contractible)).toHaveLength(1);
      }),
    });
    d.discard();
  });
  test("the two factor loops close at the same pair and wind in distinct coordinates", () => {
    for (const factor of ["first", "second"] as const) {
      const initial = torusFactorPoint(factor, 0),
        end = torusFactorPoint(factor, 1);
      end
        .flat()
        .forEach((v, i) => expect(v).toBeCloseTo(initial.flat()[i], 12));
      for (let i = 0; i <= 64; i++) {
        const p = torusFactorPoint(factor, i / 64);
        expect(Math.hypot(...p[0])).toBeCloseTo(1, 12);
        expect(Math.hypot(...p[1])).toBeCloseTo(1, 12);
        expect(p[factor === "first" ? 1 : 0]).toEqual([1, 0]);
      }
    }
  });
  test("the source torus records Z direct-sum Z and two loop classes", async () => {
    const d = await diagram({
      sub: torusFundamentalGenerators(),
      canvas: canvas(210, 131),
      sty: topology.style((ctx) => {
        expect(
          ctx.entities(topology.TorusFactorLoop).map((l) => l.factor),
        ).toEqual(["first", "second"]);
        expect(ctx.entities(topology.FreeAbelianGroup)[0].rank).toBe(2);
        const [, a, b] = ctx.facts(topology.DirectSumOf)[0];
        expect(a).toBe(b);
        expect(ctx.facts(topology.LoopClassOf)).toHaveLength(2);
        expect(ctx.facts(topology.FactorLoopOn)[0][2]).toBe(
          ctx.facts(topology.FactorLoopOn)[1][2],
        );
      }),
    });
    d.discard();
  });
});
test("basepoint conjugation has matching seams, the new basepoint, and reverse arc endpoints", async () => {
  const j = (t: number) => [1 - t, 0],
    a = (t: number) => [
      Math.cos(2 * Math.PI * t) - 1,
      Math.sin(2 * Math.PI * t),
    ];
  expect(basepointConjugateValue(j, a, 0)).toEqual([1, 0]);
  expect(basepointConjugateValue(j, a, 1)).toEqual([1, 0]);
  for (const t of [1 / 3, 2 / 3]) {
    const left = basepointConjugateValue(j, a, t - 1e-8),
      right = basepointConjugateValue(j, a, t + 1e-8);
    left.forEach((v, i) => expect(v).toBeCloseTo(right[i], 6));
  }
  expect(() => basepointConjugateValue(j, (t) => [t + 2, 0], 0.5)).toThrow();
  const d = await diagram({
    sub: transportedLoopBasepoint(),
    canvas: canvas(235, 194),
    sty: topology.style((ctx) => {
      const [transport, loop, path] = ctx.facts(
          topology.BasepointConjugateOf,
        )[0],
        [, newBase, oldBase] = ctx
          .facts(topology.PathEndpointsOf)
          .find(([p]) => p === path)!;
      expect(
        ctx.facts(topology.LoopBasedAt).find(([l]) => l === loop)![1],
      ).toBe(oldBase);
      expect(
        ctx.facts(topology.LoopBasedAt).find(([l]) => l === transport)![1],
      ).toBe(newBase);
      expect(ctx.facts(topology.LoopInverseOf)).toHaveLength(0);
      expect(ctx.facts(topology.ReversedPathOf)).toHaveLength(1);
    }),
  });
  d.discard();
});
test("homotopy inverse composites retain their true X and Y domains", async () => {
  const d = await diagram({
    sub: contractibleSingletonEquivalence(),
    canvas: canvas(314, 169),
    sty: topology.style((ctx) => {
      const [f, g, tX, tY] = ctx.facts(topology.HomotopyInverseMaps)[0],
        [k, outer, inner] = ctx.facts(topology.CompositionOf)[0];
      expect(outer).toBe(g);
      expect(inner).toBe(f);
      expect(ctx.facts(topology.CompositionOf)[1].slice(1)).toEqual([f, g]);
      const X = ctx.facts(topology.TopologyOn).find(([t]) => t === tX)![1],
        Y = ctx.facts(topology.TopologyOn).find(([t]) => t === tY)![1];
      expect(ctx.test(topology.MapBetween, k, X, X)).toBe(true);
      expect(
        ctx.test(
          topology.IdentityOn,
          ctx.facts(topology.CompositionOf)[1][0],
          Y,
        ),
      ).toBe(true);
      expect(ctx.test(topology.HomotopyEquivalent, tX, tY)).toBe(true);
    }),
  });
  d.discard();
});
test("exercise spaces have explicit graph spines and host-independent vector holes", async () => {
  expect(finiteGraphCycleRank({ vertices: 1, edges: 3, components: 1 })).toBe(
    3,
  );
  expect(() =>
    finiteGraphCycleRank({ vertices: 3, edges: 0, components: 1 }),
  ).toThrow();
  for (const [symbol, holes] of [
    ["A", 1],
    ["B", 2],
    ["C", 0],
    ["D", 1],
    ["E", 0],
    ["R", 1],
    ["T", 0],
    ["0", 1],
    ["8", 2],
  ] as const)
    expect(
      textbookGlyphOutline(symbol).filter(([cmd]) => cmd === "M"),
    ).toHaveLength(holes + 1);
  const d = await diagram({
    sub: planarHomotopyExerciseSpaces(),
    canvas: canvas(330, 118),
    sty: topology.style((ctx) => {
      expect(ctx.entities(topology.PlanarDiagramSpace)).toHaveLength(11);
      expect(ctx.facts(topology.GraphSpineOf)).toHaveLength(11);
      const [spine] = ctx
        .facts(topology.GraphSpineOf)
        .find(([, s]) => s.symbol === "108")!;
      expect(finiteGraphCycleRank(spine)).toBe(3);
      expect(ctx.facts(topology.Contractible)).toHaveLength(3);
    }),
  });
  d.discard();
});
test("all five source figures render native paths and no raster or font-dependent exercise glyphs", async () => {
  for (const build of [
    buildSphereLoopFigure,
    buildTorusGeneratorsFigure,
    buildBasepointChangeFigure,
    buildContractibleEquivalenceFigure,
    buildPlanarHomotopyExercisesFigure,
  ]) {
    const d = await build();
    try {
      while (await d.optimizationStep()) {
        // Optimize to convergence before inspecting the native geometry.
      }
      const { svg } = await d.render();
      expect(svg.querySelectorAll("image")).toHaveLength(0);
      expect(svg.querySelectorAll("path")).not.toHaveLength(0);
      if (build === buildPlanarHomotopyExercisesFigure) {
        expect(svg.querySelectorAll("text")).toHaveLength(0);
        expect(svg.querySelector('[fill-rule="evenodd"]')).not.toBeNull();
        expect(svg.outerHTML).toContain("exercise.108-top-join");
      }
    } finally {
      d.discard();
    }
  }
});

function namedShape(svg: SVGSVGElement, name: string, tag: string): Element {
  const p = Array.from(svg.querySelectorAll("title")).find(
    (t) => t.textContent === name,
  )?.parentElement;
  const result = p?.matches(tag) ? p : p?.querySelector(tag);
  if (!result) throw new Error("Missing native " + name);
  return result;
}
function pathPolygons(
  element: Element,
): readonly (readonly [number, number][])[] {
  const polygons: [number, number][][] = [];
  let polygon: [number, number][] = [];
  let previous: [number, number] = [0, 0];
  for (const command of (element.getAttribute("d") ?? "").match(
    /[MLCZ][^MLCZ]*/g,
  ) ?? []) {
    const v = (
      command.slice(1).match(/[-+]?(?:\d*\.\d+|\d+)(?:e[-+]?\d+)?/gi) ?? []
    ).map(Number);
    if (command[0] === "M") {
      if (polygon.length) polygons.push(polygon);
      polygon = [[v[0], v[1]]];
      previous = polygon[0];
    } else if (command[0] === "L") {
      previous = [v[0], v[1]];
      polygon.push(previous);
    } else if (command[0] === "C") {
      const a = previous;
      for (let i = 1; i <= 100; i++) {
        const t = i / 100,
          q = 1 - t;
        polygon.push(
          [0, 1].map(
            (j) =>
              q ** 3 * a[j] +
              3 * q * q * t * v[j] +
              3 * q * t * t * v[j + 2] +
              t ** 3 * v[j + 4],
          ) as [number, number],
        );
      }
      previous = [v[4], v[5]];
    }
  }
  if (polygon.length) polygons.push(polygon);
  return polygons;
}
function evenOddContains(element: Element, p: readonly [number, number]) {
  let inside = false;
  for (const polygon of pathPolygons(element))
    for (let i = 0, j = polygon.length - 1; i < polygon.length; j = i++) {
      const a = polygon[i],
        b = polygon[j];
      if (
        a[1] > p[1] !== b[1] > p[1] &&
        p[0] < ((b[0] - a[0]) * (p[1] - a[1])) / (b[1] - a[1]) + a[0]
      )
        inside = !inside;
    }
  return inside;
}
test("native glyph holes are transparent geometric regions, independent of contour command counts", async () => {
  const d = await buildPlanarHomotopyExercisesFigure();
  try {
    const { svg } = await d.render(),
      a = namedShape(svg, "exercise.glyph-A", "path"),
      b = namedShape(svg, "exercise.glyph-B", "path");
    expect(evenOddContains(a, [17, 18 - 26 * 0.15])).toBe(false);
    expect(evenOddContains(a, [17, 18 + 26 * 0.18])).toBe(true);
    expect(evenOddContains(b, [62, 18 - 26 * 0.2])).toBe(false);
    expect(evenOddContains(b, [62, 18 + 26 * 0.2])).toBe(false);
    expect(evenOddContains(b, [62 - 22 * 0.4, 18])).toBe(true);
  } finally {
    d.discard();
  }
});
test("the rendered torus basepoint lies on both generators and the sphere family uses native silhouette clipping", async () => {
  const a = await buildTorusGeneratorsFigure(),
    b = await buildSphereLoopFigure({
      variation: "SphereAlternate",
      interactive: true,
    });
  try {
    const { svg } = await a.render(),
      point = namedShape(svg, "torus.basepoint", "circle"),
      p = [Number(point.getAttribute("cx")), Number(point.getAttribute("cy"))];
    for (const name of [
      "torus.generator-a-visible",
      "torus.generator-b-visible",
    ]) {
      const vertices = pathPolygons(namedShape(svg, name, "path")).flat();
      expect(
        Math.min(...vertices.map((v) => Math.hypot(v[0] - p[0], v[1] - p[1]))),
      ).toBeLessThan(0.5);
    }
    const sphere = (await b.render()).svg;
    expect(sphere.querySelector("clipPath ellipse")).not.toBeNull();
    expect(sphere.outerHTML).toContain("sphere.visible-loop-family");
  } finally {
    a.discard();
    b.discard();
  }
});

/** Independently compute the graph invariant of native line/cubic centerlines after splitting crossings. */
function nativeGraphInvariant(element: Element) {
  type P = readonly [number, number];
  const edges: [P, P][] = [];
  let previous: P = [0, 0],
    first: P = previous;
  for (const command of (element.getAttribute("d") ?? "").match(
    /[MLCZ][^MLCZ]*/g,
  ) ?? []) {
    const v = (
      command.slice(1).match(/[-+]?(?:\d*\.\d+|\d+)(?:e[-+]?\d+)?/gi) ?? []
    ).map(Number);
    if (command[0] === "M") {
      first = previous = [v[0], v[1]];
    } else if (command[0] === "L") {
      const end: P = [v[0], v[1]];
      edges.push([previous, end]);
      previous = end;
    } else if (command[0] === "Z") {
      edges.push([previous, first]);
      previous = first;
    } else if (command[0] === "C") {
      const a = previous;
      for (let i = 1; i <= 30; i++) {
        const t = i / 30,
          q = 1 - t,
          end = [0, 1].map(
            (j) =>
              q ** 3 * a[j] +
              3 * q * q * t * v[j] +
              3 * q * t * t * v[j + 2] +
              t ** 3 * v[j + 4],
          ) as [number, number];
        edges.push([previous, end]);
        previous = end;
      }
    }
  }
  const split = edges.map(() => [0, 1]),
    cross = (a: P, b: P) => a[0] * b[1] - a[1] * b[0],
    minus = (a: P, b: P): P => [a[0] - b[0], a[1] - b[1]];
  for (let i = 0; i < edges.length; i++)
    for (let j = i + 1; j < edges.length; j++) {
      const [a, b] = edges[i],
        [c, d] = edges[j],
        r = minus(b, a),
        s = minus(d, c),
        den = cross(r, s);
      if (Math.abs(den) < 1e-9) continue;
      const t = cross(minus(c, a), s) / den,
        u = cross(minus(c, a), r) / den;
      if (t >= -1e-8 && t <= 1 + 1e-8 && u >= -1e-8 && u <= 1 + 1e-8) {
        split[i].push(Math.max(0, Math.min(1, t)));
        split[j].push(Math.max(0, Math.min(1, u)));
      }
    }
  const vertices = new Map<string, number>(),
    segments = new Set<string>(),
    parent: number[] = [];
  const id = (p: P) => {
    const k = p.map((v) => v.toFixed(6)).join(",");
    if (!vertices.has(k)) {
      vertices.set(k, vertices.size);
      parent.push(parent.length);
    }
    return vertices.get(k)!;
  };
  const root = (n: number): number => (parent[n] === n ? n : root(parent[n]));
  edges.forEach(([a, b], i) => {
    const ts = [...new Set(split[i].map((t) => Number(t.toFixed(9))))].sort(
      (x, y) => x - y,
    );
    const point = (t: number): P => [
      a[0] + t * (b[0] - a[0]),
      a[1] + t * (b[1] - a[1]),
    ];
    for (let j = 1; j < ts.length; j++) {
      const u = id(point(ts[j - 1])),
        v = id(point(ts[j]));
      if (u === v) continue;
      const key = [u, v].sort((x, y) => x - y).join(",");
      segments.add(key);
      parent[root(v)] = root(u);
    }
  });
  const components = new Set(parent.map((_, i) => root(i))).size;
  return { components, cycles: segments.size - vertices.size + components };
}
test("the native animal and chimney-house geometry actually has two cycles in one component", async () => {
  const d = await buildPlanarHomotopyExercisesFigure();
  try {
    const { svg } = await d.render();
    for (const name of ["exercise.animal", "exercise.house"])
      expect(nativeGraphInvariant(namedShape(svg, name, "path"))).toEqual({
        components: 1,
        cycles: 2,
      });
  } finally {
    d.discard();
  }
});

test("eleven complete exercise spaces drag intact, preserve holes and cycles, and sample reproducibly", async () => {
  const plain = await buildPlanarHomotopyExercisesFigure(),
    zero = await buildPlanarHomotopyExercisesFigure({
      interactive: { jitter: 0 },
    }),
    one = await buildPlanarHomotopyExercisesFigure({
      variation: "exercise-objects-one",
      interactive: true,
    }),
    repeat = await buildPlanarHomotopyExercisesFigure({
      variation: "exercise-objects-one",
      interactive: true,
    }),
    two = await buildPlanarHomotopyExercisesFigure({
      variation: "exercise-objects-two",
      interactive: true,
    });
  const drawings = [plain, zero, one, repeat, two];
  const geometry = (element: Element) =>
    element.matches("path,line")
      ? element
      : element.querySelector("path,line")!;
  const optimize = async (d: (typeof drawings)[number]) => {
    for (let i = 0; i < 3000; i++) if (!(await d.optimizationStep())) return;
    throw new Error("Exercise object layout did not converge");
  };
  const pathCoordinates = (element: Element) =>
    Array.from(
      (element.getAttribute("d") ?? "").matchAll(
        /[-+]?(?:\d*\.\d+|\d+)(?:e[-+]?\d+)?/gi,
      ),
      (m) => Number(m[0]),
    );
  const members = new Map([
    ...["A", "B", "C", "D", "E", "R", "T", "8"].map(
      (symbol): [string, string[]] => [symbol, [`exercise.glyph-${symbol}`]],
    ),
    ["animal", ["exercise.animal"]],
    ["house", ["exercise.house"]],
    [
      "108",
      [
        "exercise.108-1",
        "exercise.108-0",
        "exercise.108-8",
        "exercise.108-top-join",
        "exercise.108-slanted-join",
      ],
    ],
  ]);
  try {
    for (const d of drawings) await optimize(d);
    expect(plain.getDraggingConstraints().size).toBe(0);
    for (const d of drawings.slice(1)) {
      expect(Array.from(d.getDraggingConstraints().keys()).sort()).toEqual(
        Array.from(members.keys(), (id) => `exercise.space-${id}`).sort(),
      );
      const { svg } = await d.render();
      expect(svg.querySelectorAll('[data-bloom-drag="true"]')).toHaveLength(11);
      expect(svg.querySelectorAll("path[data-bloom-drag]")).toHaveLength(0);
      expect(svg.querySelectorAll("line[data-bloom-drag]")).toHaveLength(0);
    }
    const canonical = await plain.render(),
      before = await zero.render();
    for (const name of Array.from(members.values()).flat()) {
      const original = geometry(canonical.nameElemMap.get(name)!),
        untouched = geometry(before.nameElemMap.get(name)!);
      for (const attribute of ["d", "x1", "y1", "x2", "y2", "fill-rule"])
        expect(untouched.getAttribute(attribute)).toBe(
          original.getAttribute(attribute),
        );
    }
    let changedSeed = false;
    for (const symbol of members.keys()) {
      const handle = `exercise.space-${symbol}`;
      expect(zero.getInput(`${handle}.layout.x`)).toBe(0);
      expect(zero.getInput(`${handle}.layout.y`)).toBe(0);
      for (const axis of ["x", "y"]) {
        const a = one.getInput(`${handle}.layout.${axis}`),
          b = two.getInput(`${handle}.layout.${axis}`);
        expect(a).toBeCloseTo(repeat.getInput(`${handle}.layout.${axis}`), 7);
        expect(Math.abs(a)).toBeLessThanOrEqual(3);
        changedSeed ||= Math.abs(a - b) > 0.1;
      }
      zero.beginDrag(handle);
      zero.translate(handle, 2, 1);
      expect(zero.getInput(`${handle}.layout.x`)).toBeCloseTo(2, 8);
      expect(zero.getInput(`${handle}.layout.y`)).toBeCloseTo(1, 8);
      zero.endDrag(handle);
    }
    expect(changedSeed).toBe(true);
    await optimize(zero);
    const after = await zero.render();
    for (const [symbol, names] of members) {
      const dx = zero.getInput(`exercise.space-${symbol}.layout.x`),
        dy = zero.getInput(`exercise.space-${symbol}.layout.y`);
      expect(dx).toBeCloseTo(2, 2);
      expect(dy).toBeCloseTo(1, 2);
      for (const name of names) {
        const a = geometry(before.nameElemMap.get(name)!),
          b = geometry(after.nameElemMap.get(name)!);
        if (a.localName === "path") {
          const initial = pathCoordinates(a),
            moved = pathCoordinates(b);
          expect(moved).toHaveLength(initial.length);
          // SVG's vertical axis reverses Bloom's y coordinate.
          moved.forEach((v, i) =>
            expect(v - initial[i]).toBeCloseTo(i % 2 ? -dy : dx, 9),
          );
          expect(b.getAttribute("d")!.replace(/[-+\d.e\s,]/gi, "")).toBe(
            a.getAttribute("d")!.replace(/[-+\d.e\s,]/gi, ""),
          );
          expect(b.getAttribute("fill-rule")).toBe(a.getAttribute("fill-rule"));
        } else
          for (const [attribute, displacement] of [
            ["x1", dx],
            ["x2", dx],
            ["y1", -dy],
            ["y2", -dy],
          ] as const)
            expect(
              Number(b.getAttribute(attribute)) -
                Number(a.getAttribute(attribute)),
            ).toBeCloseTo(displacement, 9);
      }
    }
    for (const name of ["exercise.animal", "exercise.house"])
      expect(nativeGraphInvariant(after.nameElemMap.get(name)!)).toEqual({
        components: 1,
        cycles: 2,
      });
    const a = after.nameElemMap.get("exercise.glyph-A")!,
      b = after.nameElemMap.get("exercise.glyph-B")!,
      zeroGlyph = after.nameElemMap.get("exercise.108-0")!,
      eight = after.nameElemMap.get("exercise.108-8")!;
    expect(evenOddContains(a, [19, 17 - 26 * 0.15])).toBe(false);
    expect(evenOddContains(a, [19, 17 + 26 * 0.18])).toBe(true);
    for (const y of [17 - 26 * 0.2, 17 + 26 * 0.2])
      expect(evenOddContains(b, [64, y])).toBe(false);
    expect(evenOddContains(zeroGlyph, [284, 95])).toBe(false);
    for (const y of [95 - 26 * 0.26, 95 + 26 * 0.25])
      expect(evenOddContains(eight, [308, y])).toBe(false);
    expect(after.svg.outerHTML).not.toMatch(/NaN|Infinity/);
  } finally {
    drawings.forEach((d) => d.discard());
  }
});
