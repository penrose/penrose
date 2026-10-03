import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, test } from "vitest";
import type { Diagram } from "../core/diagram.js";
import { diagram } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { planeDistance } from "../domains/metric-spaces.js";
import {
  coordinateEmbed,
  coordinateProject,
  inIntervalProduct,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  buildProductRectangleFigure,
  buildProductStripFigure,
  buildRealLineEmbeddingFigure,
  productRectangleSubstance,
  productStripSubstance,
  realLineEmbeddingSubstance,
} from "../examples/product-topology.js";
import {
  coordinateEmbeddingStyle,
  productNeighborhoodStyle,
} from "./product-topology.js";

async function render(drawing: Diagram, id?: string) {
  try {
    for (let i = 0; i < 2000; i++) {
      if (!(await drawing.optimizationStep())) break;
      if (i === 1999)
        throw new Error("Product figure did not finish optimizing");
    }
    const { svg } = await drawing.render();
    const xml = new XMLSerializer().serializeToString(svg);
    expect(xml).not.toMatch(/NaN|undefined|Infinity/);
    expect(
      new DOMParser()
        .parseFromString(xml, "image/svg+xml")
        .querySelector("parsererror"),
    ).toBeNull();
    const destination = process.env.PENROSE_TOPOLOGY_REVIEW_DIR;
    if (destination && id) {
      mkdirSync(destination, { recursive: true });
      writeFileSync(join(destination, `figure-${id}.svg`), xml);
    }
    return svg;
  } finally {
    drawing.discard();
  }
}
const bounds = (element: Element) => {
  const values = element
    .getAttribute("points")!
    .split(/[\s,]+/)
    .map(Number);
  const xs = values.filter((_, i) => i % 2 === 0),
    ys = values.filter((_, i) => i % 2 === 1);
  return [Math.min(...xs), Math.max(...xs), Math.min(...ys), Math.max(...ys)];
};
const labels = (svg: SVGSVGElement) =>
  Array.from(svg.querySelectorAll("[data-tex]")).map((a) =>
    decodeURIComponent(a.getAttribute("data-tex")!),
  );

describe("product topology and coordinate subspaces", () => {
  test("retains open factor boundaries and the topology of the image subspace", () => {
    const interval = { a: 1, b: 2, leftClosed: false, rightClosed: false };
    const other = { a: 3, b: 4, leftClosed: false, rightClosed: false };
    for (const p of [
      [1, 3.5],
      [2, 3.5],
      [1.5, 3],
      [1.5, 4],
    ] as const)
      expect(inIntervalProduct(interval, other, p)).toBe(false);
    expect(inIntervalProduct(interval, other, [1.5, 3.5])).toBe(true);
    expect(inIntervalProduct(interval, null, [1.5, -1000])).toBe(true);
    expect(inIntervalProduct(null, other, [-1000, 3.5])).toBe(true);
    for (const varyingCoordinate of [1, 2] as const) {
      const map = { varyingCoordinate, fixedCoordinate: 0.25 };
      for (let i = -50; i <= 50; i++) {
        const x = i / 7,
          p = coordinateEmbed(map, x);
        expect(coordinateProject(varyingCoordinate, p)).toBe(x);
        expect(p[varyingCoordinate === 1 ? 1 : 0]).toBe(0.25);
        expect(
          planeDistance("euclidean", p, coordinateEmbed(map, x + 1)),
        ).toBeCloseTo(1, 10);
      }
    }
    const embedding = realLineEmbeddingSubstance();
    expect(
      embedding.propositions.filter(
        (p) => p.predicate === topology.Homeomorphism,
      ),
    ).toHaveLength(1);
    expect(
      embedding.propositions.filter(
        (p) => p.predicate === topology.CorestrictionOf,
      ),
    ).toHaveLength(1);
    for (const sub of [
      productStripSubstance(),
      productRectangleSubstance(),
      embedding,
    ])
      for (const entity of sub.entities)
        expect(entity).not.toHaveProperty("icon");
    const rectangle = productRectangleSubstance();
    const intersections = rectangle.propositions.filter(
      (p) => p.predicate === topology.FiniteIntersectionOf,
    );
    expect(intersections).toHaveLength(1);
    expect(
      rectangle.propositions.filter(
        (p) => p.predicate === topology.InverseImageOf,
      ),
    ).toHaveLength(2);
  });

  test("renders one subbasis strip and the intersection rectangle in the source compositions", async () => {
    const strip = await render(await buildProductStripFigure(), "4.8");
    const [x0, x1] = bounds(
      strip.querySelector('polygon[aria-label="product.vertical-strip"]')!,
    );
    expect(x1 - x0).toBe(84);
    expect(strip.querySelectorAll("clipPath")).toHaveLength(1);
    expect(strip.querySelectorAll("circle")).toHaveLength(2);
    expect(labels(strip)).toContain("(a,b)\\times R");
    for (const e of Array.from(strip.querySelectorAll("[data-included]")))
      expect(e.getAttribute("data-included")).toBe("false");
    const intersection = await render(
      await buildProductRectangleFigure(),
      "4.9",
    );
    const vertical = bounds(
      intersection.querySelector(
        'polygon[aria-label="product.vertical-strip"]',
      )!,
    );
    const horizontal = bounds(
      intersection.querySelector(
        'polygon[aria-label="product.horizontal-strip"]',
      )!,
    );
    const rectangle = bounds(
      intersection.querySelector('polygon[aria-label="product.rectangle"]')!,
    );
    expect(rectangle).toEqual([
      vertical[0],
      vertical[1],
      horizontal[2],
      horizontal[3],
    ]);
    expect(intersection.querySelectorAll("clipPath")).toHaveLength(2);
    expect(intersection.querySelectorAll("circle")).toHaveLength(4);
    expect(labels(intersection)).toEqual(
      expect.arrayContaining([
        "(a,b)\\times R",
        "R\\times(c,d)",
        "(a,b)\\times(c,d)",
        "(a,0)",
        "(b,0)",
        "(0,c)",
        "(0,d)",
      ]),
    );
  });

  test("renders the real-line embedding and reuses both styles on different substances", async () => {
    const embedding = await render(
      await buildRealLineEmbeddingFigure(),
      "4.10",
    );
    const image = embedding.querySelector(
      'circle[aria-label="embedding.image-point"]',
    )!;
    expect(image.getAttribute("cx")).toBe("225");
    expect(image.getAttribute("cy")).toBe("102.5");
    expect(labels(embedding)).toContain("x\\mapsto(x,0)");
    expect(
      Array.from(embedding.querySelectorAll("line[stroke-dasharray]")).filter(
        (line) => line.getAttribute("stroke-dasharray"),
      ),
    ).toHaveLength(1);
    const sty = productNeighborhoodStyle();
    const strip = await render(
      await diagram({
        sub: productStripSubstance(0.5, 1.5),
        sty,
        canvas: canvas(370, 200),
      }),
    );
    expect(
      bounds(
        strip.querySelector('polygon[aria-label="product.vertical-strip"]')!,
      ).slice(0, 2),
    ).toEqual([97, 181]);
    const rectangle = await render(
      await diagram({
        sub: productRectangleSubstance(1, 2, 0.75, 1.5),
        sty,
        canvas: canvas(365, 305),
      }),
    );
    expect(
      bounds(
        rectangle.querySelector('polygon[aria-label="product.rectangle"]')!,
      ),
    ).toEqual([179.5, 263.5, 90.5, 153.5]);
    const vertical = await render(
      await diagram({
        sub: realLineEmbeddingSubstance(0.25, 2),
        sty: coordinateEmbeddingStyle(),
        canvas: canvas(205, 310),
      }),
    );
    const marked = vertical.querySelector(
      'circle[aria-label="embedding.image-point"]',
    )!;
    expect(marked.getAttribute("cx")).toBe("109.5");
    expect(marked.getAttribute("cy")).toBe("71");
    expect(labels(vertical)).toContain("x\\mapsto(0.25,x)");
  });
});

const checkTypes = () => {
  const sub = topology.substance();
  const line = sub.RealLine(),
    plane = sub.ProductSet();
  sub.ProductOf(plane, line, line);
  // @ts-expect-error A point is not a product factor set.
  sub.ProductOf(plane, sub.RealPoint({ coordinate: 0 }), line);
  // @ts-expect-error Plane coordinate projections have only two coordinates.
  sub.CoordinateProjection({ coordinate: 3 });
};
void checkTypes;
