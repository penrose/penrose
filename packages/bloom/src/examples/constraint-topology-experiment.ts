/** Measurement instrumentation for the experimental constraint styles. */
import { collectVars, type PenroseState } from "@penrose/core";
import type { ProgramEntity } from "../core/program.js";
import { setTheory } from "../domains/set-theory.js";
import type { ConstraintTopologyStyleOptions } from "../styles/constraint-topology.js";
import {
  buildConstraintTopology,
  type ConstraintTopologyCase,
} from "./constraint-topology.js";

type Vec = readonly [number, number];
type CircleGeometry = { kind: "circle"; center: Vec; radius: number };
type PolygonGeometry = { kind: "polygon"; points: Vec[] };
type RegionGeometry = CircleGeometry | PolygonGeometry;
export interface IndependentClearance {
  label: string;
  /** Positive values are violations, in rendered SVG pixels. */
  residual: number;
}
const distance = (a: Vec, b: Vec) => Math.hypot(a[0] - b[0], a[1] - b[1]);
const segmentDistance = (p: Vec, a: Vec, b: Vec) => {
  const dx = b[0] - a[0],
    dy = b[1] - a[1];
  const square = dx * dx + dy * dy;
  const t =
    square === 0
      ? 0
      : Math.max(
          0,
          Math.min(1, ((p[0] - a[0]) * dx + (p[1] - a[1]) * dy) / square),
        );
  return distance(p, [a[0] + t * dx, a[1] + t * dy]);
};
/** Independent numeric ray casting and segment distances, outside the AD graph. */
function polygonClearance(points: Vec[], center: Vec): number {
  let inside = false,
    closest = Infinity;
  for (let i = 0; i < points.length; i++) {
    const a = points[i],
      b = points[(i + 1) % points.length];
    closest = Math.min(closest, segmentDistance(center, a, b));
    if (
      a[1] > center[1] !== b[1] > center[1] &&
      center[0] < ((b[0] - a[0]) * (center[1] - a[1])) / (b[1] - a[1]) + a[0]
    )
      inside = !inside;
  }
  return inside ? closest : -closest;
}
function readGeometry(element: Element): RegionGeometry {
  if (element.tagName.toLowerCase() === "circle")
    return {
      kind: "circle",
      center: [
        Number(element.getAttribute("cx")),
        Number(element.getAttribute("cy")),
      ],
      radius: Number(element.getAttribute("r")),
    };
  if (element.tagName.toLowerCase() === "polygon") {
    const numbers = element
      .getAttribute("points")!
      .trim()
      .split(/[\s,]+/)
      .map(Number);
    return {
      kind: "polygon",
      points: Array.from(
        { length: numbers.length / 2 },
        (_, i) => [numbers[2 * i], numbers[2 * i + 1]] as Vec,
      ),
    };
  }
  throw new Error(`Unexpected native region ${element.tagName}`);
}

export function independentTopologyClearances(
  svg: SVGElement,
  substance: Awaited<ReturnType<typeof buildConstraintTopology>>["substance"],
  padding: number,
): IndependentClearance[] {
  const regions = new Map<string, RegionGeometry>(),
    points = new Map<string, CircleGeometry>();
  for (const element of Array.from(svg.querySelectorAll("[aria-label]"))) {
    const label = element.getAttribute("aria-label")!;
    if (label.startsWith("Set "))
      regions.set(label.slice(4), readGeometry(element));
    else if (label.startsWith("Point ")) {
      const geometry = readGeometry(element);
      if (geometry.kind !== "circle")
        throw new Error("Expected native point circle");
      points.set(label.slice(6), geometry);
    }
  }
  const contain = (
    parent: RegionGeometry,
    child: CircleGeometry,
    margin: number,
  ) =>
    parent.kind === "circle"
      ? distance(parent.center, child.center) +
        child.radius +
        margin -
        parent.radius
      : child.radius + margin - polygonClearance(parent.points, child.center);
  const checks: IndependentClearance[] = [];
  for (const fact of substance.propositions) {
    const args = fact.args as readonly ProgramEntity[];
    const labels = args.map((arg) => arg.label);
    if (fact.predicate === setTheory.Subset && args[0] !== args[1]) {
      const child = regions.get(labels[0])!,
        parent = regions.get(labels[1])!;
      if (child.kind !== "circle")
        throw new Error("Unsupported independent polygon subset check");
      const reciprocal = substance.propositions.some(
        (p) =>
          p.predicate === setTheory.Subset &&
          p.args[0] === args[1] &&
          p.args[1] === args[0],
      );
      checks.push({
        label: `Subset(${labels[0]},${labels[1]})`,
        residual: contain(parent, child, reciprocal ? 0 : padding),
      });
    } else if (fact.predicate === setTheory.Member) {
      checks.push({
        label: `Member(${labels[0]},${labels[1]})`,
        residual: contain(
          regions.get(labels[1])!,
          points.get(labels[0])!,
          padding,
        ),
      });
    } else if (fact.predicate === setTheory.Intersecting) {
      const a = regions.get(labels[0])!,
        b = regions.get(labels[1])!;
      if (a.kind !== "circle" || b.kind !== "circle")
        throw new Error("Unsupported independent polygon overlap check");
      checks.push({
        label: `Intersecting(${labels[0]},${labels[1]})`,
        residual: distance(a.center, b.center) + padding - a.radius - b.radius,
      });
    } else if (fact.predicate === setTheory.Disjoint) {
      const a = regions.get(labels[0])!,
        b = regions.get(labels[1])!;
      if (a.kind !== "circle" || b.kind !== "circle")
        throw new Error("Unsupported independent noncircular disjoint check");
      checks.push({
        label: `Disjoint(${labels[0]},${labels[1]})`,
        residual: a.radius + b.radius + padding - distance(a.center, b.center),
      });
    }
  }
  return checks;
}

/** Straight-line crossings exclude edges with a shared mathematical endpoint. */
function graphMeasurements(svg: SVGElement, state: PenroseState) {
  const points = new Map<string, Vec>();
  for (const element of Array.from(
    svg.querySelectorAll("[aria-label^='Point ']"),
  )) {
    const circle = readGeometry(element) as CircleGeometry;
    points.set(element.getAttribute("aria-label")!.slice(6), circle.center);
  }
  const shapes = state.computeShapes(state.varyingValues);
  const edges = Array.from(svg.querySelectorAll("[aria-label]")).flatMap(
    (element) => {
      const match = element
        .getAttribute("aria-label")!
        .match(/^(.+): (.+) to (.+)$/);
      if (!match) return [];
      const name = element.querySelector("title")?.textContent;
      const shape = shapes.find((shape) => shape.name.contents === name);
      if (!shape || shape.shapeType !== "Line")
        throw new Error("Expected optimized native edge");
      const screen = (p: number[]): Vec => [
        p[0] + state.canvas.width / 2,
        state.canvas.height / 2 - p[1],
      ];
      const from = points.get(match[2])!,
        to = points.get(match[3])!;
      return [
        {
          label: match[0],
          source: match[2],
          target: match[3],
          from,
          to,
          incidenceDeviation: Math.max(
            distance(from, screen(shape.start.contents)),
            distance(to, screen(shape.end.contents)),
          ),
        },
      ];
    },
  );
  const orient = (a: Vec, b: Vec, c: Vec) =>
    (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0]);
  const crossings: [string, string][] = [];
  for (let i = 0; i < edges.length; i++)
    for (let j = i + 1; j < edges.length; j++) {
      const a = edges[i],
        b = edges[j];
      if (
        [a.source, a.target].some(
          (label) => label === b.source || label === b.target,
        )
      )
        continue;
      if (
        orient(a.from, a.to, b.from) * orient(a.from, a.to, b.to) < 0 &&
        orient(b.from, b.to, a.from) * orient(b.from, b.to, a.to) < 0
      )
        crossings.push([a.label, b.label]);
    }
  return {
    edgeCount: edges.length,
    crossings,
    edgeIncidenceMaxDeviation: Math.max(
      0,
      ...edges.map((edge) => edge.incidenceDeviation),
    ),
  };
}

/**
 * Runs native Penrose steps without altering solver settings. Private state is
 * read only for instrumentation, never mutated; public diagnostics provide the
 * registered residuals. SVG clearance checks are separately numeric and use
 * actual rendered circles/polygons rather than those registered expressions.
 */
export async function measureConstraintTopology(
  example: ConstraintTopologyCase,
  options: ConstraintTopologyStyleOptions = {},
  stepLimit = 2500,
) {
  const started = performance.now();
  const built = await buildConstraintTopology(example, options);
  const buildMs = performance.now() - started;
  const { drawing, styles, substance } = built;
  const initial = drawing.getConstraintDiagnostics();
  const privateState = () => Reflect.get(drawing, "state") as PenroseState;
  const startOptimization = performance.now();
  let calls = 0,
    stopped = false;
  try {
    for (; calls < stepLimit; ) {
      calls++;
      if (!(await drawing.optimizationStep())) {
        stopped = true;
        break;
      }
    }
    const optimizationMs = performance.now() - startOptimization;
    const diagnostics = drawing.getConstraintDiagnostics();
    const state = privateState();
    const { svg } = await drawing.render();
    const graph = graphMeasurements(svg, state);
    const checks = independentTopologyClearances(
      svg,
      substance,
      options.padding ?? 8,
    );
    const drift = styles.inputs.map((input) => ({
      ...input,
      final: drawing.getInput(input.name),
    }));
    const finalInput = new Map(drift.map((input) => [input.name, input.final]));
    const labelAssociations = drift
      .filter((input) => input.name.endsWith(".label.x"))
      .map((input) => {
        const name = input.name.slice(0, -8),
          isPoint = name.startsWith("constraint.point.");
        const anchor: Vec = [
          finalInput.get(`${name}.x`)! + (isPoint ? 12 : 0),
          finalInput.get(`${name}.y`)! +
            (isPoint
              ? -12
              : (finalInput.get(`${name}.r`) ??
                  finalInput.get(`${name}.height`)! / 2) + 13),
        ];
        const label: Vec = [input.final, finalInput.get(`${name}.label.y`)!];
        return { name, distance: distance(anchor, label) };
      });
    const summary = (
      role: "geometry" | "label",
      field: "initial" | "preferred",
    ) => {
      const values = drift
        .filter((input) => input.role === role)
        .map((input) => Math.abs(input.final - input[field]));
      return {
        max: Math.max(0, ...values),
        rms: Math.sqrt(values.reduce((s, x) => s + x * x, 0) / values.length),
      };
    };
    const mask = state.constraintSets.values().next().value!;
    return {
      metrics: {
        example,
        seed: options.seed ?? "book",
        perturbation: options.perturbation ?? 0,
        positionPrior: options.positionPrior ?? 0.002,
        sizePrior: options.sizePrior ?? 0.05,
        ...graph,
        labelBoundForm: options.labelBoundForm ?? "squared-distance",
        nonFiniteInputCount: state.varyingValues.filter(
          (value) => !Number.isFinite(value),
        ).length,
        nonFiniteConstraintCount: diagnostics.constraints.filter(
          (constraint) =>
            constraint.active && !Number.isFinite(constraint.value),
        ).length,
        finiteSvg: !/NaN|Infinity/.test(
          new XMLSerializer().serializeToString(svg),
        ),
        labelDistanceBound: options.labelDistanceBound ?? 24,
        arrowheadSize: options.arrowheadSize ?? 1.3,
        labelAssociations,
        maxLabelAssociationDistance: Math.max(
          0,
          ...labelAssociations.map((label) => label.distance),
        ),
        buildMs,
        optimizationMs,
        calls,
        stopped,
        stepLimit,
        status: state.params.optStatus,
        exteriorPointRounds: state.params.EPround,
        finalRoundIterations: state.params.UOround,
        optimizationFinished: diagnostics.optimizationFinished,
        feasible: diagnostics.feasible,
        tolerance: diagnostics.tolerance,
        initialMaxViolation: initial.maxViolation,
        maxViolation: diagnostics.maxViolation,
        semanticMaxViolation: Math.max(
          0,
          ...checks.map((check) => check.residual),
        ),
        geometryInputCount: styles.inputs.filter(
          (input) => input.role === "geometry",
        ).length,
        labelInputCount: styles.inputs.filter((input) => input.role === "label")
          .length,
        totalStateInputCount: state.inputs.length,
        optimizedExplicitInputCount: styles.inputs.filter((input) =>
          drawing.getOptimized(input.name),
        ).length,
        solverInputCount: mask.inputMask.filter(Boolean).length,
        activeConstraintVariableCount: (() => {
          const variables = collectVars(
            styles.registered.map((constraint) => constraint.expression),
          );
          return state.inputs.filter(
            (input, index) =>
              mask.inputMask[index] && variables.has(input.handle),
          ).length;
        })(),
        activeConstraintCount: mask.constrMask.filter(Boolean).length,
        activeObjectiveCount: mask.objMask.filter(Boolean).length,
        geometryTravel: summary("geometry", "initial"),
        geometryDriftFromBook: summary("geometry", "preferred"),
        labelTravel: summary("label", "initial"),
        labelDriftFromBook: summary("label", "preferred"),
        checks,
        drift,
        residuals: diagnostics.constraints
          .filter((c) => c.active)
          .map((c) => ({
            label: c.label,
            value: c.value,
            violation: c.violation,
          })),
        entities: substance.entities.map((e) => ({
          label: e.label,
          fields: Object.keys(e),
        })),
        facts: substance.propositions.map((p) => ({
          predicate: p.predicate.name,
          args: p.args.map((arg) =>
            "label" in arg ? arg.label : "proposition",
          ),
        })),
      },
      svg: new XMLSerializer().serializeToString(svg),
    };
  } finally {
    drawing.discard();
  }
}
