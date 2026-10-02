import { diagram, type FigureRenderOptions } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import { baireCategoryStyle } from "../styles/baire-category.js";

/** Three selected stages in the abstract metric-ball construction, with the supposed cover nested as a hypothesis. */
export function baireNestedBallsSubstance(
  radii: readonly number[] = [0.8, 0.24, 0.07],
) {
  if (
    radii.length !== 3 ||
    radii.some(
      (r, i) =>
        !Number.isFinite(r) ||
        !(r > 0 && r < 1 / (i + 1)) ||
        (i > 0 && r >= radii[i - 1] / 2),
    )
  )
    throw new Error(
      "Choose three decreasing positive radii below 1/n and half the preceding radius",
    );
  const s = topology.substance();
  const X = s.Set({ label: "X" }),
    D = s.Metric({ label: "D" }),
    tau = s.Topology({ label: "\\tau_D" });
  const family = s.CountableSetFamily({ label: "\\{A_n\\}" }),
    balls = s.CountableSetFamily({ label: "\\{B_n\\}" }),
    closedBalls = s.CountableSetFamily({
      label: "\\{\\operatorname{Cl}B_n\\}",
    });
  s.Nonempty(X);
  s.TopologyOn(tau, X);
  s.MetricOn(D, X);
  s.MetricInducesTopology(D, tau);
  s.CompleteMetricSpace(X, D);
  s.NowhereDenseFamilyIn(family, tau);
  s.DecreasingFamily(balls);
  s.DecreasingFamily(closedBalls);
  s.ClosedFamilyIn(closedBalls, X, tau);
  s.DiametersTendToZero(closedBalls, D);
  s.Hypothesis(s.FamilyUnionIs.expression(family, X));
  let previous;
  for (const [i, radius] of radii.entries()) {
    const n = i + 1,
      b = s.Point({ label: `b_${n}` }),
      A = s.Set({ label: `A_${n}` }),
      clA = s.ClosedSet({ label: `\\operatorname{Cl}A_${n}` });
    const N = s.MetricNeighborhood({ radius, label: `N(b_${n},p_${n})` }),
      B = s.MetricNeighborhood({ radius: radius / 2, label: `B_${n}` }),
      clB = s.MetricBallClosure({
        radius: radius / 2,
        label: `\\operatorname{Cl}B_${n}`,
      });
    s.SetInFamily(A, family);
    s.SetInFamily(B, balls);
    s.SetInFamily(clB, closedBalls);
    s.Subset(A, X);
    s.NowhereDenseIn(A, tau);
    s.ClosureOf(clA, A, tau);
    s.ClosedIn(clA, tau);
    s.Member(b, X);
    s.Outside(b, clA);
    s.Member(b, N);
    s.Member(b, B);
    s.Member(b, clB);
    for (const open of [N, B]) {
      s.MetricNeighborhoodAt(open, b, D);
      s.NeighborhoodOf(open, b);
      s.OpenIn(open, tau);
      s.Subset(open, X);
    }
    s.ClosureOf(clB, B, tau);
    s.ClosedIn(clB, tau);
    s.Nonempty(clB);
    s.Subset(clB, N);
    s.Disjoint(clB, clA);
    if (previous) {
      s.Member(b, previous.B);
      s.Subset(N, previous.B);
      s.Subset(B, previous.B);
      s.Subset(clB, previous.clB);
    }
    previous = { B, clB };
  }
  return s.make();
}

/** Dense open U_n and a closed complete subspace T; supposed somewhere-density is a temporary proof assumption. */
export function baireDenseIntersectionSubstance(
  p = 1,
  q = 0.22,
  qPrime = 0.045,
) {
  if (
    ![p, q, qPrime].every(Number.isFinite) ||
    !(0 < qPrime && qPrime < q && q < p / 2)
  )
    throw new Error("Choose positive p,q,q′ with q′<q<p/2");
  const s = topology.substance();
  const X = s.Set({ label: "X" }),
    D = s.Metric({ label: "D" }),
    tau = s.Topology({ label: "\\tau_D" }),
    relative = s.Topology({ label: "\\tau_T" });
  const family = s.CountableSetFamily({ label: "\\{U_n\\}" }),
    intersection = s.Set({ label: "\\bigcap_N U_n" });
  const x = s.Point({ label: "x" }),
    t = s.Point({ label: "t" }),
    z = s.Point({ label: "z" }),
    zPrime = s.Point({ label: "z'" });
  const N = s.MetricNeighborhood({ radius: p, label: "N(x,p)" }),
    half = s.MetricNeighborhood({ radius: p / 2, label: "N(x,p/2)" }),
    T = s.MetricBallClosure({
      radius: p / 2,
      label: "T=\\operatorname{Cl}N(x,p/2)",
    });
  const U = s.OpenSet({ label: "U_n" }),
    A = s.ClosedSet({ label: "T-U_n=A_n" }),
    atT = s.MetricNeighborhood({ radius: q, label: "N(t,q)" }),
    atZ = s.MetricNeighborhood({ radius: qPrime, label: "N(z,q')" }),
    local = s.Set({ label: "N(t,q)\\cap T" });
  s.Nonempty(X);
  s.MetricOn(D, X);
  s.MetricInducesTopology(D, tau);
  s.TopologyOn(tau, X);
  s.CompleteMetricSpace(X, D);
  s.TopologyOn(relative, T);
  s.SubspaceTopologyOf(relative, T, tau);
  s.MetricOn(D, T);
  s.CompleteMetricSpace(T, D);
  s.Subset(T, X);
  s.ClosedIn(T, tau);
  for (const ball of [N, half]) {
    s.MetricNeighborhoodAt(ball, x, D);
    s.NeighborhoodOf(ball, x);
    s.OpenIn(ball, tau);
    s.Member(x, ball);
  }
  s.ClosureOf(T, half, tau);
  s.Subset(T, N);
  s.Member(x, T);
  s.Member(t, T);
  s.Member(z, T);
  s.DenseOpenFamilyIn(family, tau);
  s.SetInFamily(U, family);
  s.Subset(U, X);
  s.OpenIn(U, tau);
  s.DenseIn(U, tau);
  s.FamilyIntersectionIs(family, intersection);
  s.DenseIn(intersection, tau);
  s.ComplementOf(A, U, T);
  s.ClosedIn(A, tau);
  s.ClosedIn(A, relative);
  s.NowhereDenseIn(A, relative);
  s.Hypothesis(s.SomewhereDenseIn.expression(A, relative));
  s.IntersectionOf(local, atT, T);
  s.Hypothesis(s.Subset.expression(local, A));
  s.MetricNeighborhoodAt(atT, t, D);
  s.NeighborhoodOf(atT, t);
  s.MetricNeighborhoodAt(atZ, z, D);
  s.NeighborhoodOf(atZ, z);
  s.OpenIn(atT, tau);
  s.OpenIn(atZ, tau);
  s.Member(z, atT);
  s.Member(z, half);
  s.Member(z, atZ);
  s.Member(zPrime, atZ);
  s.Member(zPrime, atT);
  s.Member(zPrime, T);
  s.Member(zPrime, U);
  s.Hypothesis(s.Member.expression(zPrime, A));
  s.Subset(atZ, atT);
  s.Subset(atZ, half);
  return s.make();
}

export const buildBaireNestedBallsFigure = (
  options: FigureRenderOptions = {},
) =>
  diagram({
    sub: baireNestedBallsSubstance(),
    sty: baireCategoryStyle(),
    canvas: canvas(300, 302),
    variation: "gemignani-10.5",
    ...options,
  });
export const buildBaireDenseIntersectionFigure = (
  options: FigureRenderOptions = {},
) =>
  diagram({
    sub: baireDenseIntersectionSubstance(),
    sty: baireCategoryStyle(),
    canvas: canvas(310, 280),
    variation: "gemignani-10.6",
    ...options,
  });
