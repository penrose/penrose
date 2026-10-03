/** @jsxImportSource @penrose/bloom */
import { add, div, max, min, sub } from "@penrose/core";
import type { InteractiveLayoutOptions } from "../core/builder.js";
import type { DomainProgram } from "../core/program.js";
import type { Circle, Equation, Rectangle } from "../core/types.js";
import {
  createFiniteFunction,
  finiteCyclicGroup,
  finiteGroupKernel,
  isFiniteGroupHomomorphism,
  setTheory,
  type ElementarySetTheoryDeclarations,
} from "../domains/set-theory.js";

export interface GroupKernelStyleOptions {
  rowSpacing?: number;
  fontSize?: string;
  /** Move a node and its label together; incident map arrows follow their inputs. */
  interactive?: false | InteractiveLayoutOptions;
}

/** A generic fiber layout for finite cyclic group homomorphisms and their kernels. */
export function groupKernelStyleFor<
  const D extends string,
  T extends ElementarySetTheoryDeclarations<D>,
>(mathematics: DomainProgram<T>, options: GroupKernelStyleOptions = {}) {
  const rowSpacing = options.rowSpacing ?? 27;
  if (!Number.isFinite(rowSpacing) || rowSpacing < 24)
    throw new Error(
      "A group-kernel layout needs finite row spacing of at least 24",
    );
  return mathematics.style((ctx) => {
    const d = mathematics.definitions;
    const homomorphisms = ctx.facts(d.GroupHomomorphismBetween);
    if (homomorphisms.length !== 1)
      throw new Error("A kernel chart needs exactly one group homomorphism");
    const [f, G, H] = homomorphisms[0];
    const cyclic = ctx.entities(d.FiniteCyclicGroup);
    const sourceEntity = cyclic.find((group) => group === G),
      targetEntity = cyclic.find((group) => group === H);
    const kernelFact = ctx.facts(d.KernelOf).find(([, map]) => map === f);
    if (
      !sourceEntity ||
      !targetEntity ||
      !kernelFact ||
      !ctx.test(d.SubgroupOf, kernelFact[0], G)
    )
      throw new Error(
        "Provide two finite cyclic groups and the stated subgroup kernel",
      );
    const K = kernelFact[0];
    const allPoints = ctx.entities(d.CyclicGroupElement);
    const sourcePoints = allPoints.filter((p) => ctx.test(d.Member, p, G));
    const targetPoints = allPoints.filter((p) => ctx.test(d.Member, p, H));
    const source = finiteCyclicGroup(sourceEntity.order),
      target = finiteCyclicGroup(targetEntity.order);
    const checkResidues = (points: typeof sourcePoints, order: number) => {
      if (
        points.length !== order ||
        new Set(points.map((p) => p.residue)).size !== order ||
        points.some(
          (p) =>
            !Number.isSafeInteger(p.residue) ||
            p.residue < 0 ||
            p.residue >= order,
        )
      )
        throw new Error("Record each distinct group residue exactly once");
    };
    checkResidues(sourcePoints, sourceEntity.order);
    checkResidues(targetPoints, targetEntity.order);
    const checkTable = (
      group: typeof G,
      points: typeof sourcePoints,
      model: typeof source,
    ) => {
      const operations = ctx
        .facts(d.GroupOperationOn)
        .filter(([, g]) => g === group);
      if (operations.length !== 1)
        throw new Error("Record one group operation for each displayed group");
      const facts = ctx
        .facts(d.ProductValue)
        .filter(([operation]) => operation === operations[0][0]);
      if (facts.length !== points.length ** 2)
        throw new Error("Record the complete finite operation table");
      for (const a of points)
        for (const b of points) {
          const results = facts
            .filter(([, x, y]) => x === a && y === b)
            .map(([, , , z]) => points.find((p) => p === z));
          if (
            results.length !== 1 ||
            results[0]?.residue !== model.operation(a.residue, b.residue)
          )
            throw new Error(
              "The operation table must agree with cyclic addition",
            );
        }
    };
    checkTable(G, sourcePoints, source);
    checkTable(H, targetPoints, target);
    const mapFacts = ctx.facts(d.MapsTo).filter(([map]) => map === f);
    if (mapFacts.length !== sourcePoints.length)
      throw new Error("Record one map value for every source element");
    const pointMap = new Map(
      sourcePoints.map((p) => {
        const values = mapFacts
          .filter(([, a]) => a === p)
          .map(([, , b]) => targetPoints.find((q) => q === b));
        if (values.length !== 1 || !values[0])
          throw new Error(
            "Map values must be unique members of the target group",
          );
        return [p, values[0]] as const;
      }),
    );
    const map = createFiniteFunction(
      source.elements,
      target.elements,
      (residue) =>
        pointMap.get(sourcePoints.find((p) => p.residue === residue)!)!.residue,
    );
    if (!isFiniteGroupHomomorphism(map, source, target))
      throw new Error(
        "The recorded values violate the complete group homomorphism law",
      );
    const kernel = finiteGroupKernel(map, source, target);
    if (
      allPoints.some(
        (p) =>
          ctx.test(d.Member, p, K) !==
          (sourcePoints.includes(p) && kernel.includes(p.residue)),
      )
    )
      throw new Error(
        "Kernel membership must be exactly the preimage of the target identity",
      );
    const orderedTargets = [...targetPoints].sort(
      (a, b) => a.residue - b.residue,
    );
    const orderedSource = [...sourcePoints].sort(
      (a, b) =>
        pointMap.get(a)!.residue - pointMap.get(b)!.residue ||
        a.residue - b.residue,
    );
    const y = (index: number) =>
      ((orderedSource.length - 1) / 2 - index) * rowSpacing;
    const ink: [number, number, number, number] = [0.08, 0.08, 0.08, 1];
    const orange: [number, number, number, number] = [0.88, 0.32, 0.08, 1];
    const label = (text: string, x: number, y: number, name: string) =>
      (
        <equation
          name={name}
          center={[x, y]}
          font-size={options.fontSize ?? "15px"}
          fill-color={ink}
          data-tex={encodeURIComponent(text)}
        >
          {text}
        </equation>
      ) as Equation;
    const targetSpacing = Math.max(
      rowSpacing,
      (orderedSource.length / orderedTargets.length) * rowSpacing,
    );
    const targetY = (i: number) =>
      ((orderedTargets.length - 1) / 2 - i) * targetSpacing;
    const top = Math.max(y(0), targetY(0)),
      bottom = -top;
    for (const [name, x, text] of [
      ["source", -170, G.label],
      ["target", 170, H.label],
    ] as const) {
      <rect
        name={`kernel.${name}-group`}
        center={[x, 0]}
        width={118}
        height={top - bottom + 58}
        corner-radius={10}
        fill-color={[1, 1, 1, 1]}
        stroke-color={[0.3, 0.3, 0.3, 1]}
        stroke-width={0.8}
      />;
      label(text, x, top + 43, `kernel.${name}-label`);
    }
    const kernelIndices = orderedSource.flatMap((p, i) =>
      kernel.includes(p.residue) ? [i] : [],
    );
    const kernelTop = y(Math.min(...kernelIndices)),
      kernelBottom = y(Math.max(...kernelIndices));
    const highlight = (
      <rect
        name="kernel.highlight"
        center={[-170, (kernelTop + kernelBottom) / 2]}
        width={100}
        height={kernelTop - kernelBottom + 23}
        corner-radius={8}
        fill-color={[0.95, 0.41, 0.12, 0.14]}
        stroke-color={orange}
        stroke-width={0.7}
      />
    ) as Rectangle;
    const positions = new Map<
      NonNullable<ReturnType<typeof pointMap.get>>,
      Circle
    >();
    const sourcePositions = new Map<(typeof orderedSource)[number], Circle>();
    const node = (
      point: (typeof orderedSource)[number],
      x: number,
      y: number,
      name: string,
      isKernel: boolean,
    ) => {
      const icon = (
        <circle
          name={name}
          center={[x, y]}
          r={9}
          fill-color={[1, 1, 1, 1]}
          stroke-color={isKernel ? orange : ink}
          stroke-width={1}
          aria-label={`Group element ${point.label}`}
        />
      ) as Circle;
      const text = label(point.label, x, y, name + ".label");
      if (options.interactive)
        ctx.builder.draggableGroup(icon, [text], {
          jitter: 0,
          maxDistance: 8,
          ...options.interactive,
        });
      return icon;
    };
    orderedSource.forEach((point, i) =>
      sourcePositions.set(
        point,
        node(
          point,
          -170,
          y(i),
          `kernel.source-${point.residue}`,
          kernel.includes(point.residue),
        ),
      ),
    );
    orderedTargets.forEach((point) => {
      positions.set(
        point,
        node(
          point,
          170,
          targetY(point.residue),
          `kernel.target-${point.residue}`,
          point.residue === target.identity,
        ),
      );
    });
    const kernelCenters = orderedSource
      .filter((p) => kernel.includes(p.residue))
      .map((p) => sourcePositions.get(p)!.center[1]);
    const upper = kernelCenters.reduce((a, b) => max(a, b)),
      lower = kernelCenters.reduce((a, b) => min(a, b));
    highlight.center[1] = div(add(upper, lower), 2);
    highlight.height = add(sub(upper, lower), 24);
    for (const point of orderedSource) {
      const a = sourcePositions.get(point)!,
        b = positions.get(pointMap.get(point)!)!;
      <line
        name={`kernel.map-${point.residue}`}
        start={[add(a.center[0], 10), a.center[1]]}
        end={[add(b.center[0], -10), b.center[1]]}
        stroke-color={kernel.includes(point.residue) ? orange : ink}
        stroke-width={0.8}
        end-arrowhead="straight"
        end-arrowhead-size={0.6}
      />;
    }
    label(f.label, 0, top + 45, "kernel.map-label");
    label("f(a+b)=f(a)+f(b)", 0, bottom - 40, "kernel.law");
    label(
      `\\ker f=\\{${kernel.map((x) => `\\overline{${x}}`).join(",")}\\}\\le ${
        G.label
      }`,
      0,
      bottom - 65,
      "kernel.formula",
    );
  });
}

export const groupKernelStyle = (options: GroupKernelStyleOptions = {}) =>
  groupKernelStyleFor(setTheory, options);
