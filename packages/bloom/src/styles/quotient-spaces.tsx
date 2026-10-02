/** @jsxImportSource @penrose/bloom */

import type { PathData } from "../core/types.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import { parametricSurfaceView, torusSurface } from "./parametric-surfaces.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

/** Two views of the same closed-subset quotient; contour geometry is schematic. */
export function closedSetCollapseStyle(
  options: TopologyStyleOptions & { view?: "source" | "quotient" } = {},
) {
  return topology.style((ctx) => {
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "16px",
    });
    const relation = ctx.entities(topology.SubsetCollapseRelation)[0];
    const construction = ctx
      .facts(topology.QuotientOf)
      .find(([, , r]) => r === relation);
    if (!relation || !construction)
      throw new Error("A closed-subset quotient is required");
    const [quotient, source] = construction;
    const tau = ctx
      .facts(topology.TopologyOn)
      .find(([, space]) => space === source)?.[0];
    const closed = ctx
      .entities(topology.ClosedSet)
      .find((set) => set === relation.collapsed);
    if (!tau || !closed || !ctx.test(topology.ClosedIn, closed, tau))
      throw new Error(
        "The collapsed subset must be closed in the source topology",
      );
    const x = ctx
      .entities(topology.Point)
      .find(
        (p) =>
          ctx.test(topology.Member, p, source) &&
          ctx.test(topology.Outside, p, relation.collapsed),
      );
    if (!x)
      throw new Error("The source needs a point outside the closed subset");
    if ((options.view ?? "source") === "source") {
      const sourceOutline: [string, ...number[]][] = [
        ["M", -39, 92],
        ["C", -70, 103, -49, 60, -47, 44],
        ["C", -40, 15, -68, -12, -64, -48],
        ["C", -60, -87, -32, -107, 3, -99],
        ["C", 44, -91, 68, -53, 65, -10],
        ["C", 68, 37, 30, 76, -39, 92],
        ["Z"],
      ];
      draw.outline(
        "quotient.source",
        sourceOutline.map(([cmd, ...coordinates]) => [
          cmd,
          ...coordinates.map((value) => value * 1.065),
        ]),
      );
      draw.hatchedArea(
        "quotient.closed-subset",
        [
          ["M", -13, 15],
          ["C", -27, 17, -23, -3, -24, -17],
          ["C", -21, -39, 4, -44, 20, -29],
          ["C", 37, -15, 29, 8, 15, 16],
          ["C", 5, 21, -3, 20, -13, 15],
          ["Z"],
        ],
        [-26, -42, 64, 64],
      );
      draw.label(relation.collapsed.label, [4, -11], true);
      draw.dot("quotient.source-point", [22, 53]);
      draw.label(x.label, [33, 55]);
      draw.label(source.label, [41, -84]);
    } else {
      draw.outline("quotient.target", [
        ["M", -25, 85],
        ["C", -63, 94, -40, 48, -41, 28],
        ["C", -35, -9, -64, -30, -53, -63],
        ["C", -41, -93, -13, -98, 20, -87],
        ["C", 54, -71, 49, -31, 43, -10],
        ["C", 42, 38, 30, 72, -25, 85],
        ["Z"],
      ]);
      const q = ctx
        .facts(topology.IdentificationMap)
        .find(
          ([, a, b, r]) => a === source && b === quotient && r === relation,
        )?.[0];
      const singleton = ctx
        .facts(topology.MapsTo)
        .find(([map, p]) => map === q && p === x)?.[2];
      if (!singleton)
        throw new Error("The quotient image of the outside point is required");
      draw.dot("quotient.singleton-image", [-8, 32]);
      draw.label(singleton.label, [-6, 48]);
      draw.dot("quotient.collapsed-class", [-2, -23]);
      draw.label(relation.collapsed.label, [13, -24]);
      draw.label(quotient.label, [41, -86]);
    }
  });
}

/** A product-of-circles view with the source's distinguished fiber y×C. */
export function circleProductStyle(
  options: TopologyStyleOptions & {
    majorRadius?: number;
    minorRadius?: number;
    elevation?: number;
  } = {},
) {
  return topology.style((ctx) => {
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "15px",
    });
    const circles = ctx.entities(topology.CircleBoundary);
    const products = ctx.facts(topology.ProductOf);
    const torus = products.find(
      ([, a, b]) => a === b && circles.some((c) => c === a),
    );
    if (!torus)
      throw new Error(
        "The surface view needs the product of one circle with itself",
      );
    const [product, circle] = torus;
    const fiber = products.find(
      ([, a, b]) =>
        b === circle && ctx.entities(topology.Singleton).some((s) => s === a),
    );
    if (!fiber || !ctx.test(topology.Subset, fiber[0], product))
      throw new Error("The product fiber must be declared as a subset");
    const point = ctx
      .facts(topology.SingletonOf)
      .find(([singleton]) => singleton === fiber[1])?.[1];
    const y = ctx.entities(topology.CoordinatePoint).find((p) => p === point);
    const factor = circles.find((c) => c === circle)!;
    if (!y || !ctx.test(topology.Member, y, factor))
      throw new Error("The fiber's fixed point must lie on its circle factor");
    const fixedAngle = Math.atan2(
      y.coordinates[1] - factor.center[1],
      y.coordinates[0] - factor.center[0],
    );
    if (
      Math.abs(
        Math.hypot(
          y.coordinates[0] - factor.center[0],
          y.coordinates[1] - factor.center[1],
        ) - factor.radius,
      ) > 1e-8
    )
      throw new Error("The fixed product coordinate must be on the circle");
    const transverse = products.find(
      ([set, a, b]) =>
        a === circle &&
        b === fiber[1] &&
        ctx.test(topology.Subset, set, product),
    );
    const shared = transverse
      ? ctx
          .facts(topology.ProductPairOf)
          .find(
            ([p, a, b]) =>
              a === y &&
              b === y &&
              ctx.test(topology.Member, p, fiber[0]) &&
              ctx.test(topology.Member, p, transverse[0]),
          )?.[0]
      : undefined;
    if (transverse && !shared)
      throw new Error(
        "Both circle fibers need their shared ordered-pair point",
      );
    // A paired-fiber diagram chooses a circle coordinate frame with its marked
    // meridian at the front. Rotational symmetry permits any fixed factor point.
    const u = transverse ? -Math.PI / 2 : fixedAngle;
    const majorRadius = options.majorRadius ?? 78,
      minorRadius = options.minorRadius ?? 25;
    const offset = draw.xy([0, 0]);
    const view = parametricSurfaceView(
      "circle-product",
      torusSurface(majorRadius * draw.scale, minorRadius * draw.scale),
      { elevation: options.elevation ?? 0.46, offset },
    );
    // The projected ring torus is the Minkowski sum of its major-circle ellipse
    // and a radius-r disk. Its analytic outer contour closes mesh contour gaps.
    const outer: PathData = [];
    const cameraSine = Math.sin(options.elevation ?? 0.46);
    if (Math.abs(cameraSine) > 1e-8) {
      for (let i = 0; i <= 192; i++) {
        const t = (2 * Math.PI * i) / 192,
          c = Math.cos(t),
          s = Math.sin(t);
        const norm = Math.hypot(cameraSine * c, s);
        outer.push({
          cmd: i === 0 ? "M" : "L",
          contents: [
            {
              tag: "CoordV",
              contents: [
                offset[0] +
                  draw.scale *
                    (majorRadius * c +
                      (minorRadius * Math.abs(cameraSine) * c) / norm),
                offset[1] +
                  draw.scale *
                    (majorRadius * s * cameraSine +
                      (minorRadius * Math.sign(cameraSine) * s) / norm),
              ],
            },
          ],
        });
      }
      outer.push({ cmd: "Z", contents: [] });
      <path
        name="circle-product.outer-contour"
        d={outer}
        fill-color={[0, 0, 0, 0]}
        stroke-color={[0.08, 0.08, 0.08, 1]}
        stroke-width={1.05}
      />;
    }
    const visible: PathData = [],
      hidden: PathData = [];
    const radius = minorRadius * draw.scale;
    const centerX = majorRadius * draw.scale * Math.cos(u) + offset[0];
    // The book widens its almost edge-on fiber to a narrow oval so the circle
    // remains legible. This minimum display width changes no product facts.
    const fiberWidth = Math.max(
      Math.abs(radius * Math.cos(u)),
      5.2 * draw.scale,
    );
    const fiberPoint = (v: number): [number, number] => {
      const p = view.project.point(view.surface(u, v).position);
      return [centerX + fiberWidth * Math.cos(v), p[1]];
    };
    let previous: PathData | undefined;
    for (let i = 0; i < 64; i++) {
      const a = fiberPoint((2 * Math.PI * i) / 64),
        b = fiberPoint((2 * Math.PI * (i + 1)) / 64);
      const mid = view.surface(u, (2 * Math.PI * (i + 0.5)) / 64);
      const front = transverse
        ? Math.cos((2 * Math.PI * (i + 0.5)) / 64) >= 0
        : view.project.facing(mid.normal) >= 0;
      const target = front ? visible : hidden;
      if (previous !== target)
        target.push({
          cmd: "M",
          contents: [{ tag: "CoordV", contents: a }],
        });
      target.push({
        cmd: "L",
        contents: [{ tag: "CoordV", contents: b }],
      });
      previous = target;
    }
    <path
      name="circle-product.fiber-hidden"
      d={hidden}
      fill-color={[0, 0, 0, 0]}
      stroke-color={[0.08, 0.08, 0.08, 1]}
      stroke-width={1.1}
      stroke-dasharray="3 3"
    />;
    <path
      name="circle-product.fiber-visible"
      d={visible}
      fill-color={[0, 0, 0, 0]}
      stroke-color={[0.08, 0.08, 0.08, 1]}
      stroke-width={1.1}
    />;
    const local = ([x, y]: [number, number]): [number, number] => [
      (x - offset[0]) / draw.scale,
      (y - offset[1]) / draw.scale,
    ];
    const centerY =
      majorRadius *
        Math.sin(u) *
        Math.sin(options.elevation ?? 0.46) *
        draw.scale +
      offset[1];
    if (transverse && shared) {
      // Gemignani uses an elliptical glyph for the transverse fiber and widens
      // the meridian. These schematic display curves preserve the shared point
      // and product facts rather than claiming an exact embedded torus latitude.
      const rx = (majorRadius - minorRadius / 2) * draw.scale;
      const ry = rx * Math.sin(options.elevation ?? 0.46);
      if (!(ry > 0))
        throw new Error(
          "The paired-fiber view needs a positive camera elevation",
        );
      <ellipse
        name="circle-product.transverse-fiber"
        center={offset}
        rx={rx}
        ry={ry}
        fill-color={[0, 0, 0, 0]}
        stroke-color={[0.08, 0.08, 0.08, 1]}
        stroke-width={1.1}
        stroke-dasharray="4 3"
      />;
      const ellipseValue = (v: number) => {
        const p = fiberPoint(v);
        return (
          ((p[0] - offset[0]) / rx) ** 2 + ((p[1] - offset[1]) / ry) ** 2 - 1
        );
      };
      let low = Math.PI,
        high = (3 * Math.PI) / 2;
      if (!(ellipseValue(low) < 0 && ellipseValue(high) > 0))
        throw new Error(
          "The paired fiber glyphs must have a front intersection",
        );
      for (let step = 0; step < 48; step++) {
        const mid = (low + high) / 2;
        if (ellipseValue(mid) < 0) low = mid;
        else high = mid;
      }
      const intersection = local(fiberPoint((low + high) / 2));
      draw.dot("circle-product.shared-point", intersection);
      draw.label(shared.label, [intersection[0] - 8, intersection[1] - 4]);
      draw.label(
        fiber[0].label,
        local([centerX + 39 * draw.scale, centerY - 4 * draw.scale]),
      );
      draw.label(transverse[0].label, [0, 43]);
    } else {
      draw.label(fiber[0].label, local([centerX + 24 * draw.scale, centerY]));
      const bottom = fiberPoint((3 * Math.PI) / 2);
      draw.label(y.label, local([bottom[0], bottom[1] - 10 * draw.scale]));
      draw.label(product.label, [51, -73]);
    }
  });
}
