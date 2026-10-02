/** @jsxImportSource @penrose/bloom */

import { add, ops } from "@penrose/core";
import * as constraints from "../core/constraints.js";
import * as objectives from "../core/objectives.js";
import type { DomainProgram } from "../core/program.js";
import type { Circle, Equation } from "../core/types.js";
import {
  setTheory,
  type SetTheoryDeclarations,
} from "../domains/set-theory.js";

type RGBA = [number, number, number, number];

export interface SetTheoryStyleOptions {
  /** Preferred radius; the optimizer can resize a set to satisfy containment. */
  preferredRadius?: number;
  minimumRadius?: number;
  pointRadius?: number;
  /** Visual clearance for distinct nested sets, membership, and disjointness. */
  padding?: number;
  regionColor?: RGBA;
  inkColor?: RGBA;
  fontSize?: string;
}

/**
 * Reuse the same visual program in a larger domain declared with
 * `declareSetTheory`. Descriptors remain owned by that larger domain.
 */
export function eulerVennStyleFor<
  const D extends string,
  T extends SetTheoryDeclarations<D>,
>(mathematics: DomainProgram<T>, options: SetTheoryStyleOptions = {}) {
  const preferredRadius = options.preferredRadius ?? 60;
  const minimumRadius = options.minimumRadius ?? 20;
  const pointRadius = options.pointRadius ?? 2.5;
  const padding = options.padding ?? 6;
  const regionColor = options.regionColor ?? [0.95, 0.41, 0.12, 0.16];
  const inkColor = options.inkColor ?? [0.08, 0.08, 0.08, 1];
  const fontSize = options.fontSize ?? "16px";
  for (const [name, value] of Object.entries({
    preferredRadius,
    minimumRadius,
    pointRadius,
    padding,
  })) {
    if (!Number.isFinite(value) || value <= 0) {
      throw new Error(`Set style requires a finite positive ${name}`);
    }
  }

  return mathematics.style((ctx) => {
    const {
      Set: SetType,
      Point,
      Subset,
      Member,
      Disjoint,
      Intersecting,
    } = mathematics.definitions;
    const sets = ctx.entities(SetType);
    const setViews = ctx.view(SetType, (set) => {
      const icon = (
        <circle
          r={ctx.input({ init: preferredRadius })}
          stroke-width={1.2}
          stroke-color={inkColor}
          fill-color={regionColor}
          aria-label={`Set ${set.label}`}
          ensure-on-canvas
        />
      ) as Circle;
      const label = (
        <equation
          center={[icon.center[0], add(add(icon.center[1], icon.r), 12)]}
          font-size={fontSize}
          fill-color={inkColor}
          ensure-on-canvas
        >
          {set.label}
        </equation>
      ) as Equation;
      ctx.ensure(constraints.greaterThan(icon.r, minimumRadius));
      ctx.encourage(objectives.equal(icon.r, preferredRadius));
      return { icon, label };
    });

    const pointViews = ctx.view(Point, (point) => {
      const icon = (
        <circle
          r={pointRadius}
          stroke-width={0}
          fill-color={inkColor}
          aria-label={`Point ${point.label}`}
          ensure-on-canvas
        />
      ) as Circle;
      const label = (
        <equation
          center={[add(icon.center[0], 10), add(icon.center[1], -10)]}
          font-size={fontSize}
          fill-color={inkColor}
          ensure-on-canvas
        >
          {point.label}
        </equation>
      ) as Equation;
      return { icon, label };
    });

    // Facts preserve their direction and include reflexive tuples. A reflexive
    // subset is already true and must not introduce positive self-containment.
    for (const [child, parent] of ctx.facts(Subset)) {
      if (child === parent) continue;
      const clearance = ctx.test(Subset, parent, child) ? 0 : padding;
      ctx.ensure(
        constraints.contains(
          setViews.get(parent).icon,
          setViews.get(child).icon,
          clearance,
        ),
      );
    }

    for (const [point, set] of ctx.facts(Member)) {
      const dot = pointViews.get(point).icon;
      const region = setViews.get(set).icon;
      ctx.ensure(constraints.contains(region, dot, padding));
      ctx.encourage(objectives.near(dot, region), 0.01);
    }

    // Mathematical symmetry is a style policy; it does not rewrite substance
    // facts. Each unordered pair contributes one optimization constraint.
    const index = new Map(sets.map((set, i) => [set, i]));
    const seenDisjoint = new Set<string>();
    const seenIntersecting = new Set<string>();
    const pairKey = (a: (typeof sets)[number], b: (typeof sets)[number]) =>
      [index.get(a)!, index.get(b)!].sort((x, y) => x - y).join(":");
    for (const [a, b] of ctx.facts(Disjoint)) {
      if (a === b) {
        throw new Error(
          "Disjoint(A, A) requires an empty-set representation; this circle style draws nonempty sets",
        );
      }
      const key = pairKey(a, b);
      if (seenDisjoint.has(key)) continue;
      seenDisjoint.add(key);
      ctx.ensure(
        constraints.disjoint(
          setViews.get(a).icon,
          setViews.get(b).icon,
          padding,
        ),
      );
    }
    for (const [a, b] of ctx.facts(Intersecting)) {
      if (a === b) continue;
      const key = pairKey(a, b);
      if (seenIntersecting.has(key)) continue;
      seenIntersecting.add(key);
      if (seenDisjoint.has(key)) {
        throw new Error("Sets cannot be both Disjoint and Intersecting");
      }
      const first = setViews.get(a).icon;
      const second = setViews.get(b).icon;
      ctx.ensure(constraints.overlapping(first, second, padding));
      if (!ctx.test(Subset, a, b) && !ctx.test(Subset, b, a)) {
        ctx.encourage(
          objectives.equal(
            ops.vdist(first.center, second.center),
            add(first.r, second.r),
          ),
          0.01,
        );
      }
    }

    const points = ctx.entities(Point).map((point) => pointViews.get(point));
    const labels = [
      ...sets.map((set) => setViews.get(set).label),
      ...points.map((point) => point.label),
    ];
    for (let i = 0; i < labels.length; i++) {
      for (let j = i + 1; j < labels.length; j++) {
        ctx.ensure(constraints.disjoint(labels[i], labels[j], 2));
      }
    }
    for (const set of sets) {
      const region = setViews.get(set).icon;
      for (const point of points) ctx.layer(region, point.icon);
      for (const label of labels) ctx.layer(region, label);
    }
    for (const point of points) {
      for (const label of labels) ctx.layer(point.icon, label);
    }
  });
}

/** A reusable circle-based Euler/Venn style for the standalone set domain. */
export const eulerVennStyle = (options: SetTheoryStyleOptions = {}) =>
  eulerVennStyleFor(setTheory, options);
