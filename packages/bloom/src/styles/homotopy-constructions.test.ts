import { describe, expect, test } from "vitest";
import type { Diagram } from "../core/diagram.js";
import { diagram, type EntityOf } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  contractAndSlideValue,
  parabolicArcValue,
  pastedHomotopyTime,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  buildContractAndSlideFigure,
  buildEndpointExtensionFigure,
  buildFixedEndpointArcsFigure,
  buildPastedHomotopyFigure,
  contractAndSlideSubstance,
  endpointExtensionSubstance,
  fixedEndpointArcsSubstance,
  pastedHomotopySubstance,
} from "../examples/homotopy-constructions.js";
import {
  contractAndSlideStyle,
  endpointExtensionStyle,
  fixedEndpointArcsStyle,
  parabolicArcControls,
  pastedHomotopyStyle,
} from "./homotopy-constructions.js";

async function render(d: Diagram) {
  let converged = false;
  for (let i = 0; i < 3000; i++)
    if (!(await d.optimizationStep())) {
      converged = true;
      break;
    }
  expect(converged).toBe(true);
  const result = await d.render();
  const xml = new XMLSerializer().serializeToString(result.svg);
  expect(xml).not.toMatch(/NaN|Infinity|undefined/);
  expect(
    new DOMParser()
      .parseFromString(xml, "image/svg+xml")
      .querySelector("parsererror"),
  ).toBeNull();
  return result;
}

describe("relative, pasted and extended homotopies", () => {
  test("the contract-and-slide family is continuous at its breakpoint and has the displayed images", () => {
    const p = [0.6, 0.8] as const;
    expect(contractAndSlideValue(p, 1)).toEqual(p);
    expect(contractAndSlideValue(p, 0.5)).toEqual([0, 0]);
    expect(contractAndSlideValue(p, 0)).toEqual([0.5, 0]);
    for (const r of [0.5 - 1e-8, 0.5 + 1e-8])
      expect(Math.hypot(...contractAndSlideValue(p, r))).toBeLessThan(3e-8);
    expect(() => contractAndSlideValue(p, 1.1)).toThrow();
    const sub = contractAndSlideSubstance();
    const H = sub.propositions.find(
      (f) => f.predicate === topology.HomotopyBetween,
    )!.args[0] as EntityOf<typeof topology.ContractAndSlideHomotopy>;
    for (const fact of sub.propositions.filter(
      (f) => f.predicate === topology.SliceMapAt,
    )) {
      expect(fact.args[1]).toBe(H);
      const r = fact.args[2] as EntityOf<typeof topology.RealPoint>;
      const image = sub.propositions.find(
        (f) => f.predicate === topology.ImageOf && f.args[1] === fact.args[0],
      )!.args[0];
      if (r.coordinate > 0.5)
        expect(
          (image as EntityOf<typeof topology.ClosedDisk>).radius,
        ).toBeCloseTo(2 * r.coordinate - 1, 12);
      else {
        const singleton = sub.propositions.find(
          (f) => f.predicate === topology.SingletonOf && f.args[0] === image,
        )!;
        const point = singleton.args[1] as EntityOf<
          typeof topology.CoordinatePoint
        >;
        expect(point.coordinates).toEqual(
          contractAndSlideValue(p, r.coordinate),
        );
      }
    }
    for (const e of sub.entities) expect(e).not.toHaveProperty("shapeType");
    expect(() => contractAndSlideSubstance(1.1)).toThrow();
  });

  test("native cubic arcs equal their mathematical quadratic paths and keep both endpoints fixed", () => {
    const sub = fixedEndpointArcsSubstance();
    const arcs = sub.propositions
      .filter((f) => f.predicate === topology.PathEndpointsOf)
      .map((f) => f.args[0] as EntityOf<typeof topology.ParabolicArc>);
    expect(arcs).toHaveLength(4);
    for (const arc of arcs) {
      const controls = parabolicArcControls(arc);
      for (const t of [0, 0.13, 0.5, 0.81, 1]) {
        const expected = parabolicArcValue(arc, t);
        for (const c of [0, 1])
          expect(
            (1 - t) ** 3 * controls[0][c] +
              3 * (1 - t) ** 2 * t * controls[1][c] +
              3 * (1 - t) * t * t * controls[2][c] +
              t ** 3 * controls[3][c],
          ).toBeCloseTo(expected[c], 12);
      }
      expect(parabolicArcValue(arc, 0)).toEqual(arc.endpoints[0]);
      expect(parabolicArcValue(arc, 1)).toEqual(arc.endpoints[1]);
    }
    const relation = sub.propositions.find(
      (f) => f.predicate === topology.HomotopyBetween,
    )!;
    const first = relation.args[1] as EntityOf<typeof topology.ParabolicArc>,
      last = relation.args[2] as EntityOf<typeof topology.ParabolicArc>;
    const deleted = sub.propositions.find(
      (f) => f.predicate === topology.DeletedPointFrom,
    )!;
    const P = deleted.args[2] as EntityOf<typeof topology.CoordinatePoint>;
    const middle = parabolicArcValue(
      { endpoints: first.endpoints, height: 0.04 },
      0.5,
    );
    expect(middle).toEqual(P.coordinates);
    expect(first.height).toBeGreaterThan(0.04);
    expect(last.height).toBeLessThan(0.04);
    expect(
      sub.propositions.some(
        (f) =>
          f.predicate === topology.RelativeHomotopyOn &&
          f.args[0] === relation.args[0],
      ),
    ).toBe(true);
    expect(
      sub.propositions.filter(
        (f) => f.predicate === topology.NotHomotopicRelativeTo,
      ),
    ).toHaveLength(1);
    expect(() => fixedEndpointArcsSubstance(0.2, -0.2, 0.3)).toThrow();
  });

  test("pasting agrees on the shared map and the extension domain is precisely both endpoint fibers", () => {
    const sub = pastedHomotopySubstance();
    const p = sub.propositions.find(
      (f) => f.predicate === topology.HomotopyPastedFrom,
    )!;
    const endpoints = (H: unknown) =>
      sub.propositions.find(
        (f) => f.predicate === topology.HomotopyBetween && f.args[0] === H,
      )!.args;
    expect(endpoints(p.args[1])[2]).toBe(endpoints(p.args[2])[1]);
    expect(endpoints(p.args[0])[1]).toBe(endpoints(p.args[1])[1]);
    expect(endpoints(p.args[0])[2]).toBe(endpoints(p.args[2])[2]);
    expect(pastedHomotopyTime(0)).toEqual({ branch: "lower", time: 0 });
    expect(pastedHomotopyTime(0.5)).toEqual({ branch: "upper", time: 0 });
    expect(pastedHomotopyTime(1)).toEqual({ branch: "upper", time: 1 });
    expect(pastedHomotopyTime(0.25)).toEqual({ branch: "lower", time: 0.5 });
    const extension = endpointExtensionSubstance();
    const fact = extension.propositions.find(
      (f) => f.predicate === topology.ExtensionOf,
    )!;
    const union = extension.propositions.find(
      (f) => f.predicate === topology.UnionOf && f.args[0] === fact.args[2],
    )!;
    const members = extension.propositions.filter(
      (f) =>
        f.predicate === topology.SetInFamily && f.args[1] === union.args[1],
    );
    expect(members).toHaveLength(2);
    expect(
      extension.propositions.some(
        (f) => f.predicate === topology.ClosedIn && f.args[0] === fact.args[2],
      ),
    ).toBe(true);
    const slices = extension.propositions.filter(
      (f) => f.predicate === topology.SliceMapAt,
    );
    expect(
      slices
        .map(
          (f) => (f.args[2] as EntityOf<typeof topology.RealPoint>).coordinate,
        )
        .sort(),
    ).toEqual([0, 1]);
    expect(() => pastedHomotopySubstance(0)).toThrow();
  });

  test("reusable styles optimize new substances and native dragging preserves all construction geometry", async () => {
    const slide = contractAndSlideStyle(),
      arcs = fixedEndpointArcsStyle(),
      paste = pastedHomotopyStyle(),
      extension = endpointExtensionStyle();
    for (const [sub, sty] of [
      [contractAndSlideSubstance(), slide],
      [contractAndSlideSubstance(0.35), slide],
      [fixedEndpointArcsSubstance(), arcs],
      [fixedEndpointArcsSubstance(0.6, -0.4, 0.1), arcs],
      [pastedHomotopySubstance(), paste],
      [pastedHomotopySubstance(0.3), paste],
      [endpointExtensionSubstance(), extension],
    ] as const) {
      const d = await diagram({ sub, sty, canvas: canvas(520, 240) });
      try {
        expect((await render(d)).svg.querySelectorAll("image")).toHaveLength(0);
      } finally {
        d.discard();
      }
    }
    for (const [factory, count] of [
      [buildContractAndSlideFigure, 11],
      [buildFixedEndpointArcsFigure, 5],
      [buildPastedHomotopyFigure, 9],
      [buildEndpointExtensionFigure, 6],
    ] as const) {
      const d = await factory({ interactive: { jitter: 0 } });
      try {
        const before = await render(d),
          handles = Array.from(d.getDraggingConstraints().keys());
        expect(handles).toHaveLength(count);
        const fixed = (svg: SVGSVGElement) =>
          Array.from(
            svg.querySelectorAll("path,line,circle,ellipse,polygon"),
            (e) => new XMLSerializer().serializeToString(e),
          );
        const geometry = fixed(before.svg),
          handle = handles[0];
        for (const h of handles) {
          expect(d.getInput(h + ".layout.x")).toBeCloseTo(0, 10);
          expect(d.getInput(h + ".layout.y")).toBeCloseTo(0, 10);
        }
        d.beginDrag(handle);
        d.translate(handle, 3, 1);
        d.endDrag(handle);
        const after = await render(d);
        expect(d.getInput(handle + ".layout.x")).toBeCloseTo(3, 4);
        expect(fixed(after.svg)).toEqual(geometry);
      } finally {
        d.discard();
      }
    }
  });
});
