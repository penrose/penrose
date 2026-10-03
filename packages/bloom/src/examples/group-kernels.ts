import { diagram, type FigureRenderOptions } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  createFiniteFunction,
  finiteCyclicGroup,
  finiteGroupKernel,
  isFiniteGroupHomomorphism,
  isFiniteSubgroup,
  setTheory,
} from "../domains/set-theory.js";
import { groupKernelStyle } from "../styles/group-kernels.js";

/**
 * A complete finite model illustrating §1.4, Exercise 2: ker(f) is a subgroup.
 * The reduction map Z/nZ→Z/mZ is a homomorphism when m divides n. Every value,
 * every operation-table entry and all kernel memberships are recorded as facts.
 */
export function cyclicGroupKernelSubstance(sourceOrder = 6, targetOrder = 3) {
  if (
    ![sourceOrder, targetOrder].every(
      (n) => Number.isSafeInteger(n) && n > 0 && n <= 16,
    )
  )
    throw new Error(
      "The displayed finite cyclic groups need orders from 1 to 16",
    );
  const source = finiteCyclicGroup(sourceOrder),
    target = finiteCyclicGroup(targetOrder);
  const reduction = createFiniteFunction(
    source.elements,
    target.elements,
    (x) => x % targetOrder,
  );
  if (!isFiniteGroupHomomorphism(reduction, source, target))
    throw new Error(
      "Reduction is a group homomorphism only when the target order divides the source order",
    );
  const kernel = finiteGroupKernel(reduction, source, target);
  if (!isFiniteSubgroup(source, kernel))
    throw new Error("The verified kernel must be a subgroup");
  const s = setTheory.substance();
  const G = s.FiniteCyclicGroup({
    order: sourceOrder,
    label: `\\mathbb{Z}_{${sourceOrder}}`,
  });
  const H = s.FiniteCyclicGroup({
    order: targetOrder,
    label: `\\mathbb{Z}_{${targetOrder}}`,
  });
  const K = s.Subgroup({ label: "\\ker f" });
  const f = s.GroupHomomorphism({ label: "f" });
  const plusG = s.BinaryOperation({ label: "+_G" }),
    plusH = s.BinaryOperation({ label: "+_H" }),
    plusK = s.BinaryOperation({ label: "+_K" });
  s.GroupHomomorphismBetween(f, G, H);
  s.MapBetween(f, G, H);
  s.Onto(f);
  s.KernelOf(K, f);
  s.SubgroupOf(K, G);
  s.Subset(K, G);
  s.GroupOperationOn(plusG, G);
  s.GroupOperationOn(plusH, H);
  s.GroupOperationOn(plusK, K);
  const sourcePoints = source.elements.map((residue) =>
    s.CyclicGroupElement({ residue, label: `\\overline{${residue}}` }),
  );
  const targetPoints = target.elements.map((residue) =>
    s.CyclicGroupElement({ residue, label: `\\overline{${residue}}` }),
  );
  for (const p of sourcePoints) {
    s.Member(p, G);
    s.MapsTo(f, p, targetPoints[reduction.apply(p.residue)]);
    s.InverseElement(p, sourcePoints[source.inverse(p.residue)], G);
    if (kernel.includes(p.residue)) s.Member(p, K);
  }
  for (const p of targetPoints) {
    s.Member(p, H);
    s.InverseElement(p, targetPoints[target.inverse(p.residue)], H);
  }
  s.IdentityElement(sourcePoints[source.identity], G);
  s.IdentityElement(targetPoints[target.identity], H);
  s.IdentityElement(sourcePoints[source.identity], K);
  for (const a of sourcePoints)
    for (const b of sourcePoints) {
      const result = sourcePoints[source.operation(a.residue, b.residue)];
      s.ProductValue(plusG, a, b, result);
      if (kernel.includes(a.residue) && kernel.includes(b.residue))
        s.ProductValue(plusK, a, b, result);
    }
  for (const a of targetPoints)
    for (const b of targetPoints)
      s.ProductValue(
        plusH,
        a,
        b,
        targetPoints[target.operation(a.residue, b.residue)],
      );
  for (const residue of kernel)
    s.InverseElement(
      sourcePoints[residue],
      sourcePoints[source.inverse(residue)],
      K,
    );
  return s.make();
}

export function buildCyclicGroupKernelFigure(
  sourceOrder = 6,
  targetOrder = 3,
  renderOptions: FigureRenderOptions = {},
) {
  const interactive = renderOptions.interactive;
  return diagram({
    sub: cyclicGroupKernelSubstance(sourceOrder, targetOrder),
    sty: groupKernelStyle({
      interactive: interactive
        ? typeof interactive === "object"
          ? interactive
          : { jitter: 0 }
        : false,
    }),
    canvas: canvas(520, sourceOrder * 27 + 145),
    variation: `original-group-kernel-${sourceOrder}-${targetOrder}`,
    ...renderOptions,
  });
}

/** Two different mathematical programs reuse the same style and coarse domain. */
export const groupKernelExamples = Object.freeze({
  z6ToZ3: () => cyclicGroupKernelSubstance(6, 3),
  z8ToZ4: () => cyclicGroupKernelSubstance(8, 4),
});
