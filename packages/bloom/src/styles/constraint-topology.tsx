/** @jsxImportSource @penrose/bloom */

import { add, mul, ops, sub, type Num } from "@penrose/core";
import * as constraints from "../core/constraints.js";
import * as objectives from "../core/objectives.js";
import type { ProgramEntity, ProgramStyleContext } from "../core/program.js";
import type { Circle, Equation, Line, Polygon, Vec2 } from "../core/types.js";
import { setTheory } from "../domains/set-theory.js";

export interface ConstraintRegionHint {
  center: readonly [number, number];
  /** A circle radius or the full width/height of a simple L-shaped polygon. */
  size: number | readonly [number, number];
  kind?: "circle" | "L";
}
export interface ConstraintTopologyStyleOptions {
  regions?: Readonly<Record<string, ConstraintRegionHint>>;
  points?: Readonly<Record<string, readonly [number, number]>>;
  seed?: string;
  /** Perturb initialized coordinates; this never changes Substance facts. */
  perturbation?: number;
  /** Explicit weak aesthetic preference, not an equality constraint. */
  positionPrior?: number;
  sizePrior?: number;
  padding?: number;
  pointRadius?: number;
  fontSize?: number;
  /** Visual association bound relative to the live object, never a book coordinate. */
  labelDistanceBound?: number | false;
  arrowheadSize?: number;
  /** Distance is retained only to reproduce the initial experiment. */
  labelBoundForm?: "distance" | "squared-distance";
}
export interface ConstraintTopologyInput {
  name: string;
  initial: number;
  preferred: number;
  role: "geometry" | "label";
}
type Context = ProgramStyleContext<typeof setTheory.definitions>;
type RegionView = { icon: Circle | Polygon; center: Vec2; label: Equation };
type PointView = { icon: Circle; label: Equation };
type Views = {
  regions: Map<ProgramEntity, RegionView>;
  points: Map<ProgramEntity, PointView>;
  labels: Equation[];
};
const ink: [number, number, number, number] = [0.13, 0.12, 0.11, 1];
const orange: [number, number, number, number] = [0.95, 0.41, 0.12, 0.14];

/** A deterministic initialization sampler; optimization still uses Penrose. */
const sampler = (seed: string) => {
  let value = 2166136261;
  for (const char of seed)
    value = Math.imul(value ^ char.charCodeAt(0), 16777619);
  return () => {
    value ^= value << 13;
    value ^= value >>> 17;
    value ^= value << 5;
    return (value >>> 0) / 4294967296;
  };
};

/**
 * Two styles share fresh views inside each diagram assembly. Region/point
 * geometry is optimized; relations attach to those very same optimized nodes.
 * Circle containment and membership have positive visual clearance. The L
 * template is a finite simple polygon; its translation, width, and height are
 * optimized, while its topology and notch proportions are aesthetic choices.
 *
 * Only recorded positive facts are enforced. Unasserted membership is unknown.
 * This does not imply all intersections, reconstruct arbitrary set algebra,
 * prove topological facts, or require graph edges to remain inside nonconvex
 * regions. Empty sets, self edges and polygon-in-polygon containment need other
 * representations and are rejected when requested.
 */
export function constraintTopologyStyles(
  options: ConstraintTopologyStyleOptions = {},
) {
  const amplitude = options.perturbation ?? 0;
  const positionPrior = options.positionPrior ?? 0.002;
  const sizePrior = options.sizePrior ?? 0.05;
  const padding = options.padding ?? 8;
  const pointRadius = options.pointRadius ?? 4;
  const fontSize = options.fontSize ?? 15;
  const labelDistanceBound = options.labelDistanceBound ?? 24;
  const arrowheadSize = options.arrowheadSize ?? 1.3;
  const labelBoundForm = options.labelBoundForm ?? "squared-distance";
  for (const [name, value] of Object.entries({
    amplitude,
    positionPrior,
    sizePrior,
    padding,
    pointRadius,
    fontSize,
    arrowheadSize,
    ...(labelDistanceBound === false ? {} : { labelDistanceBound }),
  })) {
    if (
      !Number.isFinite(value) ||
      value < 0 ||
      (["pointRadius", "fontSize"].includes(name) && value === 0)
    )
      throw new Error(`Invalid constraint topology ${name}`);
  }
  const perContext = new WeakMap<Context, Views>();
  const inputs: ConstraintTopologyInput[] = [];
  const registered: { label: string; expression: Num }[] = [];
  let random = sampler(options.seed ?? "book");
  const register = (ctx: Context, expression: Num, label: string) => {
    registered.push({ expression, label });
    ctx.ensure(expression, undefined, label);
  };
  const input = (
    ctx: Context,
    name: string,
    preferred: number,
    role: ConstraintTopologyInput["role"],
    jitter = amplitude,
  ) => {
    const initial = preferred + (2 * random() - 1) * jitter;
    inputs.push({ name, initial, preferred, role });
    return ctx.input({ name, init: initial, optimized: true });
  };

  const regions = setTheory.style((ctx) => {
    random = sampler(options.seed ?? "book");
    // A mathematical relation is a set of ordered pairs; this style draws its
    // recorded arrows rather than adding a misleading region for that set.
    const relations = new Set(ctx.entities(setTheory.BinaryRelation));
    const functions = new Set(ctx.entities(setTheory.Function));
    const sets = ctx
      .entities(setTheory.Set)
      .filter((set) => !relations.has(set as never));
    const points = ctx
      .entities(setTheory.Point)
      .filter((point) => !functions.has(point as never));
    const views: Views = { regions: new Map(), points: new Map(), labels: [] };
    perContext.set(ctx, views);
    const label = (
      text: string,
      preferred: Vec2,
      initial: readonly [number, number],
      name: string,
    ) => {
      const center: Vec2 = [
        input(ctx, `${name}.label.x`, initial[0], "label"),
        input(ctx, `${name}.label.y`, initial[1], "label"),
      ];
      const shape = (
        <equation
          name={`${name}.label`}
          center={center}
          font-size={`${fontSize}px`}
          fill-color={ink}
          ensure-on-canvas
        >
          {text}
        </equation>
      ) as Equation;
      ctx.encourage(objectives.equal(center[0], preferred[0]), 0.02);
      ctx.encourage(objectives.equal(center[1], preferred[1]), 0.02);
      if (labelDistanceBound !== false)
        register(
          ctx,
          labelBoundForm === "distance"
            ? constraints.lessThan(
                ops.vdist(center, preferred),
                labelDistanceBound,
              )
            : mul(
                1 / Math.max(2 * labelDistanceBound, 1),
                sub(ops.vdistsq(center, preferred), labelDistanceBound ** 2),
              ),
          `visual:label association ${text}`,
        );
      views.labels.push(shape);
      return shape;
    };
    sets.forEach((set, i) => {
      const hint = options.regions?.[set.label] ?? {
        center: [i * 130 - (sets.length - 1) * 65, 0],
        size: 70,
      };
      const dimensions =
        typeof hint.size === "number" ? [hint.size, hint.size] : hint.size;
      if (
        ![...hint.center, ...dimensions].every(Number.isFinite) ||
        dimensions.some((v) => v <= 0)
      )
        throw new Error(`Invalid region initialization for ${set.label}`);
      const name = `constraint.region.${i}`;
      const center: Vec2 = [
        input(ctx, `${name}.x`, hint.center[0], "geometry"),
        input(ctx, `${name}.y`, hint.center[1], "geometry"),
      ];
      if (positionPrior > 0)
        for (let k = 0; k < 2; k++)
          ctx.encourage(
            objectives.equal(center[k], hint.center[k]),
            positionPrior,
          );
      let icon: Circle | Polygon;
      let above: Num;
      if ((hint.kind ?? "circle") === "circle") {
        const radius = input(
          ctx,
          `${name}.r`,
          dimensions[0],
          "geometry",
          Math.min(amplitude / 4, dimensions[0] / 2),
        );
        icon = (
          <circle
            name={name}
            center={center}
            r={radius}
            stroke-width={1.1}
            stroke-color={ink}
            fill-color={orange}
            aria-label={`Set ${set.label}`}
            ensure-on-canvas
          />
        ) as Circle;
        register(
          ctx,
          constraints.greaterThan(radius, 20),
          `visual:positive radius ${set.label}`,
        );
        if (sizePrior > 0)
          ctx.encourage(objectives.equal(radius, dimensions[0]), sizePrior);
        above = add(center[1], add(radius, 13));
      } else {
        const width = input(
          ctx,
          `${name}.width`,
          dimensions[0],
          "geometry",
          Math.min(amplitude / 4, dimensions[0] / 3),
        );
        const height = input(
          ctx,
          `${name}.height`,
          dimensions[1],
          "geometry",
          Math.min(amplitude / 4, dimensions[1] / 3),
        );
        register(
          ctx,
          constraints.greaterThan(width, 70),
          `visual:positive width ${set.label}`,
        );
        register(
          ctx,
          constraints.greaterThan(height, 70),
          `visual:positive height ${set.label}`,
        );
        if (sizePrior > 0) {
          ctx.encourage(objectives.equal(width, dimensions[0]), sizePrior);
          ctx.encourage(objectives.equal(height, dimensions[1]), sizePrior);
        }
        const vertex = (x: number, y: number): Vec2 => [
          add(center[0], mul(width, x)),
          add(center[1], mul(height, y)),
        ];
        // Clockwise or counterclockwise, with no repeated edges or hidden holes.
        icon = (
          <polygon
            name={name}
            points={[
              vertex(-0.5, -0.5),
              vertex(0.5, -0.5),
              vertex(0.5, -0.1),
              vertex(-0.1, -0.1),
              vertex(-0.1, 0.5),
              vertex(-0.5, 0.5),
            ]}
            scale={1}
            stroke-width={1.1}
            stroke-color={ink}
            fill-color={orange}
            aria-label={`Set ${set.label}`}
            ensure-on-canvas
          />
        ) as Polygon;
        above = add(center[1], add(mul(height, 0.5), 13));
      }
      const initialY =
        hint.center[1] +
        ((hint.kind ?? "circle") === "L" ? dimensions[1] / 2 : dimensions[0]) +
        13;
      views.regions.set(set, {
        icon,
        center,
        label: label(
          set.label,
          [center[0], above],
          [hint.center[0], initialY],
          name,
        ),
      });
    });
    points.forEach((point, i) => {
      const preferred = options.points?.[point.label] ?? [
        i * 35 - (points.length - 1) * 17.5,
        -10,
      ];
      if (!preferred.every(Number.isFinite))
        throw new Error(`Invalid point initialization for ${point.label}`);
      const name = `constraint.point.${i}`;
      const center: Vec2 = [
        input(ctx, `${name}.x`, preferred[0], "geometry"),
        input(ctx, `${name}.y`, preferred[1], "geometry"),
      ];
      const icon = (
        <circle
          name={name}
          center={center}
          r={pointRadius}
          stroke-width={0}
          fill-color={ink}
          aria-label={`Point ${point.label}`}
          ensure-on-canvas
        />
      ) as Circle;
      if (positionPrior > 0)
        for (let k = 0; k < 2; k++)
          ctx.encourage(
            objectives.equal(center[k], preferred[k]),
            positionPrior,
          );
      const annotation = label(
        point.label,
        [add(center[0], 12), add(center[1], -12)],
        [preferred[0] + 12, preferred[1] - 12],
        name,
      );
      views.points.set(point, { icon, label: annotation });
    });
    const region = (entity: ProgramEntity) => {
      const view = views.regions.get(entity);
      if (!view)
        throw new Error(
          `This region representation does not draw ${entity.label}`,
        );
      return view.icon;
    };
    for (const [child, parent] of ctx.facts(setTheory.Subset)) {
      if (child === parent) continue;
      const a = region(parent),
        b = region(child);
      const clearance = ctx.test(setTheory.Subset, parent, child) ? 0 : padding;
      if (a.shapeType === "Polygon" && b.shapeType === "Polygon")
        throw new Error(
          "Polygon-in-polygon subset requires an exact containment representation",
        );
      const expression =
        a.shapeType === "Polygon" && b.shapeType === "Circle"
          ? constraints.containsPolyCircle(a.points, b.center, b.r, clearance)
          : constraints.contains(a, b, clearance);
      register(
        ctx,
        expression,
        `semantic:Subset(${child.label},${parent.label})`,
      );
    }
    for (const [point, set] of ctx.facts(setTheory.Member)) {
      const p = views.points.get(point)?.icon;
      if (!p)
        throw new Error(
          `This point representation does not draw ${point.label}`,
        );
      const a = region(set);
      register(
        ctx,
        a.shapeType === "Polygon"
          ? constraints.containsPolyCircle(a.points, p.center, p.r, padding)
          : constraints.contains(a, p, padding),
        `semantic:Member(${point.label},${set.label})`,
      );
    }
    for (const [a, b] of ctx.facts(setTheory.Disjoint)) {
      if (a === b)
        throw new Error(
          "An empty-set representation is required for Disjoint(A,A)",
        );
      register(
        ctx,
        constraints.disjoint(region(a), region(b), padding),
        `semantic:Disjoint(${a.label},${b.label})`,
      );
    }
    for (const [a, b] of ctx.facts(setTheory.Intersecting)) {
      if (a !== b)
        register(
          ctx,
          constraints.overlapping(region(a), region(b), padding),
          `semantic:Intersecting(${a.label},${b.label})`,
        );
    }
    const dots = [...views.points.values()].map((point) => point.icon);
    for (let i = 0; i < dots.length; i++)
      for (let j = i + 1; j < dots.length; j++)
        register(
          ctx,
          constraints.disjoint(dots[i], dots[j], 8),
          `visual:distinct drawn points ${i},${j}`,
        );
    for (let i = 0; i < views.labels.length; i++) {
      for (let j = i + 1; j < views.labels.length; j++)
        register(
          ctx,
          constraints.disjoint(views.labels[i], views.labels[j], 3),
          `visual:label clearance ${i},${j}`,
        );
      for (let j = 0; j < dots.length; j++)
        register(
          ctx,
          constraints.disjoint(views.labels[i], dots[j], 3),
          `visual:label-point clearance ${i},${j}`,
        );
    }
    for (const a of views.regions.values())
      for (const p of views.points.values()) ctx.layer(a.icon, p.icon);
    for (const a of views.regions.values())
      for (const l of views.labels) ctx.layer(a.icon, l);
  });

  const relations = setTheory.style((ctx) => {
    const views = perContext.get(ctx);
    if (!views)
      throw new Error(
        "Compose the region style before the relation style from the same bundle",
      );
    const edges = [
      ...ctx.facts(setTheory.RelatedUnder),
      ...ctx.facts(setTheory.MapsTo),
    ];
    const seen = new Set<string>();
    for (const [relation, source, target] of edges) {
      if (source === target)
        throw new Error("Self edges require a loop representation");
      const from = views.points.get(source),
        to = views.points.get(target);
      if (!from || !to)
        throw new Error("Relation endpoints require drawn points");
      const key = `${ctx.substance.entities.indexOf(
        relation,
      )}:${ctx.substance.entities.indexOf(
        source,
      )}:${ctx.substance.entities.indexOf(target)}`;
      if (seen.has(key)) continue;
      seen.add(key);
      const edge = (
        <line
          name={`constraint.edge.${seen.size}`}
          start={from.icon.center}
          end={to.icon.center}
          stroke-color={ink}
          stroke-width={0.9}
          end-arrowhead="straight"
          end-arrowhead-size={arrowheadSize}
          aria-label={`${relation.label}: ${source.label} to ${target.label}`}
          ensure-on-canvas
        />
      ) as Line;
      for (const [point, view] of views.points) {
        if (point !== source && point !== target)
          register(
            ctx,
            constraints.disjoint(edge, view.icon, 6),
            `visual:edge ${source.label}→${target.label} avoids ${point.label}`,
          );
        ctx.layer(edge, view.icon);
      }
      for (const view of views.regions.values()) ctx.layer(view.icon, edge);
      for (let i = 0; i < views.labels.length; i++) {
        register(
          ctx,
          constraints.disjoint(edge, views.labels[i], 2),
          `visual:edge ${source.label}→${target.label} label clearance ${i}`,
        );
        ctx.layer(edge, views.labels[i]);
      }
    }
  });
  return { regions, relations, inputs, registered };
}
