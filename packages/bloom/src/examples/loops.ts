import {
  diagram,
  type EntityOf,
  type FigureRenderOptions,
} from "../core/program.js";
import { canvas } from "../core/utils.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  inverseLoopStyle,
  loopFamilyStyle,
  loopOperationStyle,
  loopStyle,
} from "../styles/loops.js";

function basedLoopBuilder() {
  const s = topology.substance();
  const Y = s.EuclideanPlane({ label: "Y" }),
    tauY = s.Topology({ label: "\\tau_Y" });
  const I = s.ClosedInterval({
    a: 0,
    b: 1,
    leftClosed: true,
    rightClosed: true,
    label: "[0,1]",
  });
  const boundary = s.EndpointPair({ endpoints: [0, 1], label: "\\{0,1\\}" });
  const square = s.ProductSet({ label: "[0,1]\\times[0,1]" });
  const tauI = s.Topology({ label: "\\tau_I" }),
    tauSquare = s.Topology({ label: "\\tau_{I\\times I}" });
  const y0 = s.CoordinatePoint({ coordinates: [0, 0], label: "y_0" });
  const loops = s.LoopSpace({ label: "L(Y,y_0)" }),
    G = s.FundamentalGroup({ label: "\\pi_1(Y,y_0)" });
  s.TopologyOn(tauY, Y);
  s.TopologyOn(tauI, I);
  s.TopologyOn(tauSquare, square);
  s.Member(y0, Y);
  s.Subset(boundary, I);
  s.ClosedIn(boundary, tauI);
  s.ProductOf(square, I, I);
  s.ProductTopologyOf(tauSquare, tauI, tauI);
  s.LoopSpaceOf(loops, tauY, y0);
  s.FundamentalGroupOf(G, tauY, y0);
  const based = (loop: EntityOf<typeof topology.Loop>) => {
    s.LoopBasedAt(loop, y0, tauY);
    s.LoopInSpace(loop, loops);
    s.MapBetween(loop, I, Y);
    s.ContinuousMap(loop, tauI, tauY);
    s.PathEndpointsOf(loop, y0, y0);
    const cls = s.LoopHomotopyClass({ label: "|" + loop.label + "|" });
    s.LoopClassOf(cls, loop, tauY, y0);
    s.ClassInFundamentalGroup(cls, G);
    return cls;
  };
  const homotopy = (
    H: EntityOf<typeof topology.Homotopy>,
    one: EntityOf<typeof topology.Loop>,
    zero: EntityOf<typeof topology.Loop>,
  ) => {
    s.MapBetween(H, square, Y);
    s.ContinuousMap(H, tauSquare, tauY);
    s.HomotopyBetween(H, one, zero);
    s.RelativeHomotopyOn(H, boundary);
    s.RelativelyHomotopic(one, zero, boundary, tauY);
  };
  return {
    s,
    Y,
    tauY,
    I,
    boundary,
    square,
    tauI,
    tauSquare,
    y0,
    G,
    based,
    homotopy,
  };
}

/** A loop begins and ends at the same point; its drawn self-intersections do not change this condition. */
export function basedLoopSubstance() {
  const { s, based } = basedLoopBuilder();
  based(s.Loop({ label: "a" }));
  return s.make();
}

/** A family of finite Fourier loops fixes the same basepoint at every parameter. */
export function basedLoopFamilySubstance(innerScale = 0.58, parameter = 0.5) {
  if (
    ![innerScale, parameter].every(Number.isFinite) ||
    !(innerScale > 0 && innerScale < 1 && parameter > 0 && parameter < 1)
  )
    throw new Error(
      "A loop family needs an inner scale and parameter strictly between0 and1",
    );
  const { s, Y, I, y0, based, homotopy } = basedLoopBuilder();
  const outerData = {
    constant: [-0.0933608564, 0.3797032045],
    cosine: [
      [0.0999178102, -0.4055919932],
      [-0.0109915862, -0.0028223351],
      [0.0048698261, 0.0199197161],
      [0.0023825778, 0.0053206857],
      [-0.0013658692, 0.0025487296],
      [0.0003934324, 0.003336626],
      [-0.0008317784, -0.0006374757],
      [-0.0010135563, -0.0017771579],
    ],
    sine: [
      [0.4018116403, 0.2063030483],
      [-0.0086005793, 0.0122781107],
      [-0.0051922293, 0.0100588757],
      [0.0011660739, 0.007670287],
      [-0.0043133121, -0.0024867353],
      [-0.0013036103, -0.0019222361],
      [0.0009887427, 0.0007234415],
      [-0.0011377552, -0.000587357],
    ],
  } as const;
  const innerData = {
    constant: [-0.0756482099, 0.1994753908],
    cosine: [
      [0.0535945821, -0.1875075985],
      [0.014597338, -0.0107252788],
      [0.0053640267, 0.0134819983],
      [-0.001851582, -0.0059710027],
      [-0.0005794352, -0.0045810862],
      [0.0025184972, -0.0002251432],
      [0.0020093795, -0.0027095114],
      [-4.5964e-6, -0.0012377683],
    ],
    sine: [
      [0.2053808833, 0.1410066394],
      [-0.0025549392, -0.0092529045],
      [-0.0092505874, 0.0017550646],
      [0.0037202147, -0.000825048],
      [-0.0024464866, -0.0014820356],
      [-0.0007377366, 0.0008138074],
      [0.0008669064, 0.0010422862],
      [-0.0008640391, 0.0001270864],
    ],
  } as const;
  const blend = (one: typeof outerData, zero: typeof innerData, r: number) => ({
    constant: one.constant.map(
      (v, i) => r * v + (1 - r) * zero.constant[i] * (innerScale / 0.58),
    ) as [number, number],
    cosine: one.cosine.map(
      (p, n) =>
        p.map(
          (v, i) => r * v + (1 - r) * zero.cosine[n][i] * (innerScale / 0.58),
        ) as [number, number],
    ),
    sine: one.sine.map(
      (p, n) =>
        p.map(
          (v, i) => r * v + (1 - r) * zero.sine[n][i] * (innerScale / 0.58),
        ) as [number, number],
    ),
  });
  const first = s.TrigonometricLoop({ ...outerData, label: "a_1" }),
    last = s.TrigonometricLoop({
      ...blend(outerData, innerData, 0),
      label: "a_0",
    }),
    intermediate = s.TrigonometricLoop({
      ...blend(outerData, innerData, parameter),
      label: "a_r",
    });
  const firstClass = based(first),
    lastClass = based(last);
  based(intermediate);
  s.TopologicalEqualSets(firstClass, lastClass);
  const H = s.Homotopy({
    label: "H",
    formula: "H(u,r)=r a₁(u)+(1−r)a₀(u)",
  });
  homotopy(H, first, last);
  for (const [time, loop, label] of [
    [0, last, "0"],
    [parameter, intermediate, "r"],
    [1, first, "1"],
  ] as const) {
    const r = s.RealPoint({ coordinate: time, label });
    s.Member(r, I);
    s.SliceMapAt(loop, H, r);
    const image = s.Set({ label: loop.label + "([0,1])" });
    s.ImageOf(image, loop, I);
    s.Subset(image, Y);
    for (const time of [0, 1]) {
      const endpoint = s.RealPoint({ coordinate: time, label: "" });
      s.Member(endpoint, I);
      s.MapsTo(loop, endpoint, y0);
    }
  }
  return s.make();
}

/** Concatenation descends to based homotopy classes by holding its second loop fixed. */
export function concatenationHomotopySubstance() {
  const { s, G, based, homotopy } = basedLoopBuilder();
  const a1 = s.Loop({ label: "a_1" }),
    a2 = s.Loop({ label: "a_2" }),
    a3 = s.Loop({ label: "a_3" });
  const c1 = based(a1),
    c2 = based(a2),
    c3 = based(a3);
  const H = s.Homotopy({ label: "H" });
  homotopy(H, a1, a3);
  s.TopologicalEqualSets(c1, c3);
  const first = s.LoopConcatenation({ label: "a_1\\mathbin{\\#}a_2" }),
    last = s.LoopConcatenation({ label: "a_3\\mathbin{\\#}a_2" });
  const fClass = based(first),
    lClass = based(last);
  s.LoopProductOf(first, a1, a2);
  s.LoopProductOf(last, a3, a2);
  s.LoopClassProductOf(fClass, c1, c2, G);
  s.LoopClassProductOf(lClass, c3, c2, G);
  s.TopologicalEqualSets(fClass, lClass);
  const Hprime = s.Homotopy({
    label: "H'",
    formula: "r≤1/2:H(2r,s); r≥1/2:a₂(2r−1)",
  });
  homotopy(Hprime, first, last);
  return s.make();
}

/** The changing durations(1+s)/4,1/4,(2−s)/4 realize associativity relative to the basepoint. */
export function associativeLoopSubstance() {
  const { s, G, based, homotopy } = basedLoopBuilder();
  const a1 = s.Loop({ label: "a_1" }),
    a2 = s.Loop({ label: "a_2" }),
    a3 = s.Loop({ label: "a_3" });
  const c1 = based(a1),
    c2 = based(a2),
    c3 = based(a3);
  const a12 = s.LoopConcatenation({ label: "a_1\\mathbin{\\#}a_2" }),
    a23 = s.LoopConcatenation({ label: "a_2\\mathbin{\\#}a_3" });
  const c12 = based(a12),
    c23 = based(a23);
  s.LoopProductOf(a12, a1, a2);
  s.LoopProductOf(a23, a2, a3);
  s.LoopClassProductOf(c12, c1, c2, G);
  s.LoopClassProductOf(c23, c2, c3, G);
  const left = s.LoopConcatenation({
      label: "(a_1\\mathbin{\\#}a_2)\\mathbin{\\#}a_3",
    }),
    right = s.LoopConcatenation({
      label: "a_1\\mathbin{\\#}(a_2\\mathbin{\\#}a_3)",
    });
  const cLeft = based(left),
    cRight = based(right);
  s.LoopProductOf(left, a12, a3);
  s.LoopProductOf(right, a1, a23);
  s.LoopClassProductOf(cLeft, c12, c3, G);
  s.LoopClassProductOf(cRight, c1, c23, G);
  s.TopologicalEqualSets(cLeft, cRight);
  const H = s.Homotopy({
    label: "H",
    formula: "a₁(4r/(1+s)); a₂(4r−1−s); a₃(1−4(1−r)/(2−s))",
  });
  homotopy(H, right, left);
  return s.make();
}

/** A constant first or last interval shrinks continuously, showing the loop class identity. */
export function unitLoopSubstance(side: "left" | "right" = "right") {
  const { s, y0, tauY, G, based, homotopy } = basedLoopBuilder();
  const a = s.Loop({ label: "a_1" }),
    k = s.ConstantLoop({ label: "k" });
  const ca = based(a),
    ck = based(k);
  s.ConstantLoopAt(k, y0, tauY);
  s.IdentityElement(ck, G);
  const product = s.LoopConcatenation({
    label: side === "right" ? "a_1\\mathbin{\\#}k" : "k\\mathbin{\\#}a_1",
  });
  const cp = based(product);
  s.LoopProductOf(product, side === "right" ? a : k, side === "right" ? k : a);
  s.LoopClassProductOf(
    cp,
    side === "right" ? ca : ck,
    side === "right" ? ck : ca,
    G,
  );
  s.TopologicalEqualSets(cp, ca);
  const H = s.Homotopy({
    label: "H",
    formula:
      side === "right" ? "a₁(min(r/(1−s/2),1))" : "a₁(max((r−s/2)/(1−s/2),0))",
  });
  homotopy(H, product, a);
  return s.make();
}

/** Reversing the parameter traverses the same circle in the opposite direction. */
export function inverseLoopSubstance() {
  const { s, Y, I, y0, G, based } = basedLoopBuilder();
  const circle = s.CircleBoundary({ center: [0, 1], radius: 1, label: "" });
  s.Subset(circle, Y);
  s.Member(y0, circle);
  const a = s.TrigonometricLoop({
    constant: [0, 1],
    cosine: [[0, -1]],
    sine: [[1, 0]],
    label: "a_1",
  });
  const inverse = s.InverseLoop({ label: "a_1^{-1}", formula: "r↦a₁(1−r)" });
  const ca = based(a),
    ci = based(inverse);
  s.LoopInverseOf(inverse, a);
  s.InverseElement(ci, ca, G);
  for (const loop of [a, inverse]) s.ImageOf(circle, loop, I);
  return s.make();
}

export const buildBasedLoopFigure = (renderOptions: FigureRenderOptions = {}) =>
  diagram({
    sub: basedLoopSubstance(),
    sty: loopStyle(),
    canvas: canvas(162, 122),
    variation: "gemignani-11.9",
    ...renderOptions,
  });
export const buildBasedLoopFamilyFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: basedLoopFamilySubstance(),
    sty: loopFamilyStyle(),
    canvas: canvas(224, 134),
    variation: "gemignani-11.10",
    ...renderOptions,
  });
export const buildConcatenationHomotopyFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: concatenationHomotopySubstance(),
    sty: loopOperationStyle({ offset: [0, 4] }),
    canvas: canvas(120, 142),
    variation: "gemignani-11.11",
    ...renderOptions,
  });
export const buildAssociativeLoopFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: associativeLoopSubstance(),
    sty: loopOperationStyle({ offset: [-4, 0] }),
    canvas: canvas(218, 168),
    variation: "gemignani-11.12",
    ...renderOptions,
  });
export const buildRightUnitLoopFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: unitLoopSubstance("right"),
    sty: loopOperationStyle(),
    canvas: canvas(106, 120),
    variation: "gemignani-11.13",
    ...renderOptions,
  });
export const buildLeftUnitLoopFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: unitLoopSubstance("left"),
    sty: loopOperationStyle(),
    canvas: canvas(106, 120),
    variation: "gemignani-11.14",
    ...renderOptions,
  });
export const buildInverseLoopFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: inverseLoopSubstance(),
    sty: inverseLoopStyle(),
    canvas: canvas(208, 106),
    variation: "gemignani-11.15",
    ...renderOptions,
  });
