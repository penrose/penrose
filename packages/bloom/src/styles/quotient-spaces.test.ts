import { expect, test } from "vitest";
import { diagram, type EntityOf } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  circleProductSubstance,
  closedSubsetQuotientSubstance,
} from "../examples/quotient-spaces.js";
import { surfaceProjection, torusSurface } from "./parametric-surfaces.js";
import {
  circleProductStyle,
  closedSetCollapseStyle,
} from "./quotient-spaces.js";

test("the closed subset has one quotient class while an outside point remains a singleton", () => {
  const sub = closedSubsetQuotientSubstance();
  const relation = sub.entities.find((e) => e.label === "R") as EntityOf<
    typeof topology.SubsetCollapseRelation
  >;
  expect(sub.entities).toContain(relation.collapsed);
  const members = sub.propositions.filter(
    (p) => p.predicate === topology.ClassOf,
  );
  expect(members).toHaveLength(2);
  expect(members[0].args[0]).not.toBe(members[1].args[0]);
  const map = sub.propositions.find(
    (p) => p.predicate === topology.IdentificationMap,
  )!.args[0];
  const images = sub.propositions.filter(
    (p) => p.predicate === topology.MapsTo && p.args[0] === map,
  );
  expect(
    images.map(
      (p) => (p.args[2] as EntityOf<typeof topology.EquivalenceClass>).label,
    ),
  ).toEqual(["\\{x\\}=\\bar{x}", "F"]);
  expect(
    sub.propositions.filter((p) => p.predicate === topology.T2),
  ).toHaveLength(1);
});

test("a product-of-circles substance records a singleton factor fiber without drawing data", () => {
  for (const angle of [-Math.PI / 2, 0.8]) {
    const sub = circleProductSubstance(angle);
    const products = sub.propositions.filter(
      (p) => p.predicate === topology.ProductOf,
    );
    expect(products).toHaveLength(2);
    expect(products[0].args[1]).toBe(products[0].args[2]);
    expect(products[1].args[2]).toBe(products[0].args[1]);
    const point = sub.entities.find((e) => e.label === "y") as EntityOf<
      typeof topology.CoordinatePoint
    >;
    expect(Math.hypot(...point.coordinates)).toBeCloseTo(1, 12);
    expect(point).not.toHaveProperty("center");
  }
});

test("both compact product fibers meet in an ordered pair, distinct from their factor point", async () => {
  const sharedStyle = circleProductStyle();
  for (const angle of [-Math.PI / 2, 0.8]) {
    const sub = circleProductSubstance(angle, {
      fixedLabel: "x",
      bothFibers: true,
      compact: true,
    });
    const pair = sub.propositions.find(
      (p) => p.predicate === topology.ProductPairOf,
    )!;
    expect(pair.args[1]).toBe(pair.args[2]);
    expect(pair.args[0]).not.toBe(pair.args[1]);
    expect(
      sub.propositions.filter((p) => p.predicate === topology.Compact),
    ).toHaveLength(2);
    const products = sub.propositions.filter(
      (p) => p.predicate === topology.ProductOf,
    );
    expect(products).toHaveLength(3);
    for (const p of products)
      expect(
        sub.propositions.some(
          (f) =>
            f.predicate === topology.Member &&
            f.args[0] === pair.args[0] &&
            f.args[1] === p.args[0],
        ),
      ).toBe(true);
    expect(
      sub.propositions.some(
        (f) =>
          f.predicate === topology.Member &&
          f.args[0] === pair.args[1] &&
          f.args[1] === products[0].args[0],
      ),
    ).toBe(false);
    const drawing = await diagram({
      sub,
      sty: sharedStyle,
      canvas: canvas(216, 134),
      interactive: { jitter: 0 },
    });
    try {
      while (await drawing.optimizationStep()) {
        // Complete native optimization before inspecting the rendered incidence.
      }
      const { svg, nameElemMap } = await drawing.render();
      expect(svg.querySelectorAll("polygon")).toHaveLength(1536);
      const transverse = nameElemMap.get("circle-product.transverse-fiber")!;
      expect(transverse.getAttribute("stroke-dasharray")).toBe("4 3");
      const hidden = nameElemMap.get("circle-product.fiber-hidden")!;
      // Restarting each short segment would reset dash phase and look solid.
      expect(hidden.getAttribute("d")!.match(/M/g)).toHaveLength(1);
      const point = nameElemMap.get("circle-product.shared-point")!;
      const cx = Number(transverse.getAttribute("cx")),
        cy = Number(transverse.getAttribute("cy"));
      const rx = Number(transverse.getAttribute("rx")),
        ry = Number(transverse.getAttribute("ry"));
      const x = Number(point.getAttribute("cx")),
        y = Number(point.getAttribute("cy"));
      expect(((x - cx) / rx) ** 2 + ((y - cy) / ry) ** 2).toBeCloseTo(1, 9);
      expect(drawing.getDraggingConstraints().size).toBe(3);
      expect(
        new DOMParser()
          .parseFromString(
            new XMLSerializer().serializeToString(svg),
            "image/svg+xml",
          )
          .querySelector("parsererror"),
      ).toBeNull();
    } finally {
      drawing.discard();
    }
  }
});

test("the reusable torus parametrization is periodic and has correct radial geometry and normals", () => {
  const surface = torusSurface(3, 1);
  for (let u = 0; u < 6; u += 0.4)
    for (let v = 0; v < 6; v += 0.4) {
      const {
        position: [x, y, z],
        normal,
      } = surface(u, v);
      expect((Math.hypot(x, y) - 3) ** 2 + z * z).toBeCloseTo(1, 10);
      expect(Math.hypot(...normal)).toBeCloseTo(1, 12);
      surface(u + 2 * Math.PI, v + 2 * Math.PI).position.forEach((p, i) =>
        expect(p).toBeCloseTo(surface(u, v).position[i], 10),
      );
    }
  expect(() => torusSurface(1, 2)).toThrow();
  const view = surfaceProjection(0.46);
  expect(view.depth([0, -1, 0])).toBeLessThan(view.depth([0, 1, 0]));
});

test("the same quotient instance renders both source and target as XML-valid native geometry", async () => {
  const sub = closedSubsetQuotientSubstance();
  for (const view of ["source", "quotient"] as const) {
    const drawing = await diagram({
      sub,
      sty: closedSetCollapseStyle({ view }),
      canvas: canvas(160, 230),
    });
    try {
      const { svg } = await drawing.render();
      expect(
        svg.querySelector(
          `[aria-label="quotient.${view === "source" ? "source" : "target"}"]`,
        ),
      ).not.toBeNull();
      const xml = new XMLSerializer().serializeToString(svg);
      expect(
        new DOMParser()
          .parseFromString(xml, "image/svg+xml")
          .querySelector("parsererror"),
      ).toBeNull();
      expect(xml).not.toMatch(/NaN|undefined/);
    } finally {
      drawing.discard();
    }
  }
});

test("the product style produces native shaded polygons, visible contours and a split fiber", async () => {
  const drawing = await diagram({
    sub: circleProductSubstance(),
    sty: circleProductStyle(),
    canvas: canvas(226, 164),
  });
  try {
    const { svg } = await drawing.render();
    expect(svg.querySelectorAll("polygon").length).toBeGreaterThan(500);
    expect(
      svg.querySelector('[aria-label="circle-product shaded surface"]'),
    ).not.toBeNull();
    const paths = Array.from(svg.querySelectorAll("path"));
    expect(
      paths.some((p) => p.getAttribute("stroke-dasharray") === "3 3"),
    ).toBe(true);
    const xml = new XMLSerializer().serializeToString(svg);
    expect(
      new DOMParser()
        .parseFromString(xml, "image/svg+xml")
        .querySelector("parsererror"),
    ).toBeNull();
    expect(xml).not.toMatch(/NaN|undefined/);
  } finally {
    drawing.discard();
  }
});
