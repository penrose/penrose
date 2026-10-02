/** @jsxImportSource @penrose/bloom */

import type { ProgramStyleContext } from "../core/program.js";
import type { PathData } from "../core/types.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  surfaceProjection,
  type ParametricSurface,
} from "./parametric-surfaces.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";
import { hatchedTopologyPolygon } from "./topology-bases.js";

type Context = ProgramStyleContext<typeof topology.definitions>;
type XY = [number, number];

/** A geometric view of a circle parameter times an interval parameter, with no end caps. */
export function cylinderSurface(
  radius: number,
  height: number,
): ParametricSurface {
  if (![radius, height].every(Number.isFinite) || !(radius > 0 && height > 0))
    throw new Error("Cylinder view dimensions must be finite and positive");
  return (angle, t) => ({
    position: [
      radius * Math.cos(angle),
      radius * Math.sin(angle),
      height * (t - 0.5),
    ],
    normal: [Math.cos(angle), Math.sin(angle), 0],
  });
}

function requireCompact(ctx: Context, product: object) {
  const tau = ctx
    .facts(topology.TopologyOn)
    .find(([, set]) => set === product)?.[0];
  if (!tau || !ctx.test(topology.Compact, tau))
    throw new Error(
      "The product surface must have an asserted compact topology",
    );
  return tau;
}

/** Native product-surface views recognize I×C and (I×I)×I from mathematical facts. */
export function compactProductSurfaceStyle(
  options: TopologyStyleOptions & {
    radius?: number;
    height?: number;
    elevation?: number;
  } = {},
) {
  return topology.style((ctx) => {
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "10px",
    });
    const products = ctx.facts(topology.ProductOf);
    const intervals = ctx.entities(topology.ClosedInterval);
    const circles = ctx.entities(topology.CircleBoundary);
    const cylinder = products.find(
      ([, a, b]) =>
        intervals.some((i) => i === a) && circles.some((c) => c === b),
    );
    if (cylinder) {
      const [product, intervalEntity, circleEntity] = cylinder;
      const I = intervals.find((i) => i === intervalEntity)!;
      const C = circles.find((c) => c === circleEntity)!;
      if (
        !(I.a < I.b && C.radius > 0) ||
        ![I.a, I.b, ...C.center, C.radius].every(Number.isFinite)
      )
        throw new Error(
          "The cylinder factors require a nondegenerate interval and circle",
        );
      const tau = requireCompact(ctx, product);
      const tauI = requireCompact(ctx, I);
      const tauC = requireCompact(ctx, C);
      if (!ctx.test(topology.ProductTopologyOf, tau, tauI, tauC))
        throw new Error("The compact cylinder must carry the product topology");
      const endpoint = (coordinate: number) => {
        const fact = ctx
          .facts(topology.SingletonOf)
          .find(([, point]) =>
            ctx
              .entities(topology.RealPoint)
              .some((p) => p === point && p.coordinate === coordinate),
          );
        const fiber =
          fact && products.find(([, a, b]) => a === fact[0] && b === C);
        if (
          !fact ||
          !fiber ||
          !ctx.test(topology.Subset, fiber[0], product) ||
          !ctx.test(topology.CompactIn, fiber[0], tau)
        )
          throw new Error(
            "The cylinder needs both compact endpoint circle fibers",
          );
        return { fiber: fiber[0], point: fact[1] };
      };
      const bottom = endpoint(I.a),
        top = endpoint(I.b);
      const marked = products.find(
        ([, a, b]) =>
          a === I && ctx.entities(topology.Singleton).some((s) => s === b),
      );
      const fixed =
        marked &&
        ctx.facts(topology.SingletonOf).find(([s]) => s === marked[2])?.[1];
      const x = ctx.entities(topology.CoordinatePoint).find((p) => p === fixed);
      if (
        !marked ||
        !x ||
        !ctx.test(topology.Member, x, C) ||
        !x.coordinates.every(Number.isFinite) ||
        !ctx.test(topology.Subset, marked[0], product) ||
        Math.abs(
          Math.hypot(
            x.coordinates[0] - C.center[0],
            x.coordinates[1] - C.center[1],
          ) - C.radius,
        ) > 1e-8
      )
        throw new Error(
          "The marked interval fiber must use a point on the circle",
        );
      const radius = (options.radius ?? 40) * draw.scale;
      const height = (options.height ?? 86) * draw.scale;
      const elevation = options.elevation ?? 0.43;
      if (!(elevation > 0 && elevation < Math.PI / 2))
        throw new Error("Cylinder elevation must lie between zero and pi/2");
      const surface = cylinderSurface(radius, height);
      const view = surfaceProjection(elevation, draw.xy([0, 0]));
      const color = options.regionColor ?? [0.95, 0.41, 0.12, 0.14];
      // Only the near lateral half is colored. White endpoint openings preserve
      // the source's wireframe occlusion convention and do not add disk factors.
      for (let i = 0; i < 48; i++) {
        const a = -Math.PI + (Math.PI * i) / 48;
        const b = -Math.PI + (Math.PI * (i + 1)) / 48;
        const shade = 0.55 + 0.45 * Math.abs(Math.cos((a + b) / 2));
        <polygon
          points={[
            [a, 0],
            [b, 0],
            [b, 1],
            [a, 1],
          ].map(([u, v]) => view.point(surface(u, v).position))}
          fill-color={[color[0], color[1], color[2], color[3] * shade]}
          stroke-width={0}
          ensure-on-canvas={false}
        />;
      }
      for (const t of [0, 1])
        <ellipse
          name={t === 0 ? "cylinder.bottom-circle" : "cylinder.top-circle"}
          center={
            view
              .point(surface(0, t).position)
              .map((v, i) => (i === 0 ? v - radius : v)) as XY
          }
          rx={radius}
          ry={radius * Math.sin(elevation)}
          fill-color={[1, 1, 1, 1]}
          stroke-color={[0.08, 0.08, 0.08, 1]}
          stroke-width={1.1}
          aria-label={
            t === 0 ? "cylinder.bottom-circle" : "cylinder.top-circle"
          }
        />;
      const local = ([a, b]: XY): XY => {
        const origin = draw.xy([0, 0]);
        return [(a - origin[0]) / draw.scale, (b - origin[1]) / draw.scale];
      };
      for (const angle of [0, Math.PI])
        draw.line(
          "cylinder.side-" + angle,
          local(view.point(surface(angle, 0).position)),
          local(view.point(surface(angle, 1).position)),
        );
      const angle = Math.atan2(
        x.coordinates[1] - C.center[1],
        x.coordinates[0] - C.center[0],
      );
      const at = local(view.point(surface(angle, 1).position));
      draw.line(
        "cylinder.marked-interval-fiber",
        local(view.point(surface(angle, 0).position)),
        at,
      );
      draw.dot("cylinder.marked-circle-point", at);
      draw.label(x.label, [at[0] - 9, at[1] + 1]);
      const halfHeight = (height * Math.cos(elevation)) / (2 * draw.scale);
      const r = radius / draw.scale;
      const ry = r * Math.sin(elevation);
      draw.label(top.point.label, [r + 7, halfHeight]);
      draw.label(bottom.point.label, [r + 7, -halfHeight]);
      draw.dot("cylinder.upper-interval-endpoint", [r, halfHeight]);
      draw.label(top.fiber.label, [0, halfHeight + ry + 10]);
      draw.label(bottom.fiber.label, [0, -halfHeight - ry - 12]);
      draw.label(marked[0].label, [-23, 1]);
      return;
    }
    const square = products.find(
      ([, a, b]) => a === b && intervals.some((i) => i === a),
    );
    const cube =
      square && products.find(([, a, b]) => a === square[0] && b === square[1]);
    if (!square || !cube)
      throw new Error("The compact product style requires I×C or I×I×I");
    const I = intervals.find((i) => i === square[1])!;
    if (!(I.a < I.b) || ![I.a, I.b].every(Number.isFinite))
      throw new Error(
        "Cube factors require a finite nondegenerate closed interval",
      );
    const tau = requireCompact(ctx, cube[0]);
    const tauI = requireCompact(ctx, I);
    const tauSquare = requireCompact(ctx, square[0]);
    if (
      !ctx.test(topology.ProductTopologyOf, tauSquare, tauI, tauI) ||
      !ctx.test(topology.ProductTopologyOf, tau, tauSquare, tauI)
    )
      throw new Error(
        "The compact cube must carry its iterated product topology",
      );
    const endpoint = (coordinate: number) => {
      const fact = ctx
        .facts(topology.SingletonOf)
        .find(([, point]) =>
          ctx
            .entities(topology.RealPoint)
            .some((p) => p === point && p.coordinate === coordinate),
        );
      if (!fact) throw new Error("Cube endpoint singleton is missing");
      return fact;
    };
    const lower = endpoint(I.a),
      upper = endpoint(I.b);
    const firstBase = products.find(([, a, b]) => a === lower[0] && b === I);
    const left =
      firstBase && products.find(([, a, b]) => a === firstBase[0] && b === I);
    const top = products.find(([, a, b]) => a === square[0] && b === upper[0]);
    if (
      !left ||
      !top ||
      !ctx.test(topology.Subset, left[0], cube[0]) ||
      !ctx.test(topology.Subset, top[0], cube[0]) ||
      !ctx.test(topology.CompactIn, left[0], tau) ||
      !ctx.test(topology.CompactIn, top[0], tau)
    )
      throw new Error(
        "The two compact boundary faces must be asserted as cube subsets",
      );
    const project = ([x, y, z]: readonly [number, number, number]): XY =>
      draw.xy([-47 + 56 * x + 44 * y, -40 - 17 * x + 38 * y + 68 * z]);
    const A = project([0, 0, 0]),
      B = project([1, 0, 0]),
      C = project([1, 1, 0]),
      D = project([0, 1, 0]);
    const a = project([0, 0, 1]),
      b = project([1, 0, 1]),
      c = project([1, 1, 1]),
      d = project([0, 1, 1]);
    const color = options.regionColor ?? [0.95, 0.41, 0.12, 0.14];
    hatchedTopologyPolygon(
      "cube.first-coordinate-face",
      [A, D, d, a],
      Math.PI / 2,
      color,
      { spacing: 2.2, strokeWidth: 0.6, strokeOpacity: 0.85 },
    );
    hatchedTopologyPolygon(
      "cube.last-coordinate-face",
      [a, b, c, d],
      Math.atan2(-17, 56),
      color,
      { spacing: 2.6, strokeWidth: 0.6, strokeOpacity: 0.85 },
    );
    // The source continues its vertical face hatching into this part of the
    // top face, producing its distinctive crosshatching near the back vertex.
    hatchedTopologyPolygon(
      "cube.top-crosshatch",
      [a, d, project([44 / 56, 0, 1])],
      Math.PI / 2,
      [0, 0, 0, 0],
      { spacing: 2.2, strokeWidth: 0.6, strokeOpacity: 0.85 },
    );
    const edges: PathData = [];
    for (const [from, to] of [
      [A, B],
      [B, C],
      [C, D],
      [D, A],
      [a, b],
      [b, c],
      [c, d],
      [d, a],
      [A, a],
      [B, b],
      [C, c],
      [D, d],
    ])
      edges.push(
        { cmd: "M", contents: [{ tag: "CoordV", contents: from }] },
        { cmd: "L", contents: [{ tag: "CoordV", contents: to }] },
      );
    <path
      name="cube.wireframe"
      d={edges}
      fill-color={[0, 0, 0, 0]}
      stroke-color={[0.08, 0.08, 0.08, 1]}
      stroke-width={1.1}
      aria-label="cube twelve edges"
    />;
    const label = (text: string, at: XY) => {
      <rect
        center={draw.xy(at)}
        width={50 * draw.scale}
        height={12 * draw.scale}
        fill-color={[1, 1, 1, 1]}
        stroke-width={0}
      />;
      draw.label(text, at);
    };
    <polyline
      points={[
        [-10, -31],
        [-10, -14],
        [-23, -9],
      ].map((p) => draw.xy(p as XY))}
      fill-color={[0, 0, 0, 0]}
      stroke-color={[0.08, 0.08, 0.08, 1]}
      stroke-width={0.9}
      aria-label="cube first-coordinate-face leader"
    />;
    <polygon
      points={[
        [-23, -9],
        [-19.2, -8.8],
        [-20.3, -11.8],
      ].map((p) => draw.xy(p as XY))}
      fill-color={[0.08, 0.08, 0.08, 1]}
      stroke-width={0}
      aria-label="cube first-coordinate-face arrow pointing left"
    />;
    label(top[0].label, [3, 38]);
    label(left[0].label, [-11, -39]);
    draw.label(lower[1].label, [-54, -43]);
    draw.label(upper[1].label, [-54, 28]);
  });
}
