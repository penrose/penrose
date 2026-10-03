import { diagram, type FigureRenderOptions } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  circularBasedLoop,
  piecewisePlaneLoopData,
  planeCurveSegmentValue,
  planeLoopDiskBound,
  type CircularPlaneSegment,
  type CubicPlaneSegment,
  type PiecewisePlaneLoopData,
  type PlaneCurvePoint,
} from "../domains/plane-curves.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import { planeLoopFamilyStyle } from "../styles/plane-curves.js";

const COVER_BASE: PlaneCurvePoint = [(351 - 526) / 353, (895 - 1018) / 353];
const unit = (x: number, y: number): PlaneCurvePoint => [
  (x - 526) / 353,
  (895 - y) / 353,
];
const cubic = (
  a: PlaneCurvePoint,
  b: PlaneCurvePoint,
  c: PlaneCurvePoint,
  d: PlaneCurvePoint,
): CubicPlaneSegment => ({ kind: "cubic", controls: [a, b, c, d] });

/**
 * A mathematical reconstruction of the visible cover fan, in a unit-disk chart.
 * The source does not identify its formula or artist intent. These closed cubic/circular
 * curves reproduce the visible geometry; they are not attributed to an original formula.
 */
export function coverFanCurveData(): readonly PiecewisePlaneLoopData[] {
  const B = COVER_BASE;
  const petal = (segments: readonly CubicPlaneSegment[]) =>
    piecewisePlaneLoopData(segments);
  const boundaryPetal = (
    outerPixel: PlaneCurvePoint,
    returnPixel: PlaneCurvePoint,
    outward: readonly [PlaneCurvePoint, PlaneCurvePoint],
    inward: readonly [PlaneCurvePoint, PlaneCurvePoint],
  ) => {
    const startAngle = Math.atan2(895 - outerPixel[1], outerPixel[0] - 526),
      endAngle = Math.atan2(895 - returnPixel[1], returnPixel[0] - 526),
      arc: CircularPlaneSegment = {
        kind: "arc",
        center: [0, 0],
        radius: 1,
        startAngle,
        sweep: endAngle - startAngle,
      },
      from = planeCurveSegmentValue(arc, 0),
      to = planeCurveSegmentValue(arc, 1);
    return piecewisePlaneLoopData([
      cubic(B, unit(...outward[0]), unit(...outward[1]), from),
      arc,
      cubic(to, unit(...inward[0]), unit(...inward[1]), B),
    ]);
  };
  return Object.freeze([
    petal([
      cubic(B, unit(270, 1015), unit(211, 985), unit(201, 909)),
      cubic(unit(201, 909), unit(178, 789), unit(252, 654), unit(315, 634)),
      cubic(unit(315, 634), unit(436, 592), unit(371, 846), B),
    ]),
    petal([
      cubic(B, unit(269, 962), unit(235, 889), unit(252, 821)),
      cubic(unit(252, 821), unit(273, 736), unit(330, 761), unit(333, 825)),
      cubic(unit(333, 825), unit(338, 889), unit(331, 973), B),
    ]),
    petal([
      cubic(B, unit(311, 982), unit(274, 917), unit(289, 882)),
      cubic(unit(289, 882), unit(306, 839), unit(318, 911), unit(321, 940)),
      cubic(unit(321, 940), unit(327, 976), unit(340, 1005), B),
    ]),
    boundaryPetal(
      [786, 654],
      [218, 1068],
      [
        [410, 805],
        [606, 537],
      ],
      [
        [259, 1030],
        [308, 1010],
      ],
    ),
    boundaryPetal(
      [876, 836],
      [269, 1133],
      [
        [503, 825],
        [726, 662],
      ],
      [
        [268, 1084],
        [302, 1043],
      ],
    ),
    boundaryPetal(
      [853, 1017],
      [338, 1197],
      [
        [520, 881],
        [824, 850],
      ],
      [
        [320, 1122],
        [330, 1066],
      ],
    ),
    boundaryPetal(
      [776, 1141],
      [588, 1243],
      [
        [584, 1008],
        [784, 1005],
      ],
      [
        [484, 1241],
        [393, 1105],
      ],
    ),
    petal([
      cubic(B, unit(491, 1030), unit(690, 1040), unit(728, 1124)),
      cubic(unit(728, 1124), unit(769, 1209), unit(643, 1217), unit(559, 1179)),
      cubic(unit(559, 1179), unit(475, 1141), unit(392, 1090), B),
    ]),
  ]);
}

function loopFamily(
  loops: readonly PiecewisePlaneLoopData[],
  base: PlaneCurvePoint,
) {
  const s = topology.substance(),
    disk = s.ClosedDisk({ label: "D", center: [0, 0], radius: 1 }),
    tau = s.Topology({ label: "\\tau_D" }),
    p = s.CoordinatePoint({ label: "p", coordinates: base }),
    family = s.PlaneLoopFamily({ label: "\\mathcal L" });
  s.TopologyOn(tau, disk);
  s.Member(p, disk);
  for (const [i, data] of loops.entries()) {
    if (!planeLoopDiskBound(data, disk.center, disk.radius).contained)
      throw new Error(
        "Every plane-family loop needs whole-curve disk containment",
      );
    const loop = s.PiecewisePlaneLoop({ label: `a_${i + 1}`, ...data }),
      constant = s.ConstantLoop({ label: `k_${i + 1}` }),
      contraction = s.Homotopy({ label: `H_${i + 1}` });
    s.LoopBasedAt(loop, p, tau);
    s.LoopInPlaneFamily(loop, family);
    s.ConstantLoopAt(constant, p, tau);
    s.LoopBasedAt(constant, p, tau);
    // Book convention: HomotopyBetween(H,f,g) has H(-,1)=f and H(-,0)=g.
    s.HomotopyBetween(contraction, constant, loop);
    s.NullHomotopic(loop, tau, p);
  }
  return s.make();
}
/** Geometry is mathematical curve data; this Substance program declares no visual shapes. */
export function coverLoopFan() {
  return loopFamily(coverFanCurveData(), COVER_BASE);
}

/** A distinct exact circle pencil reuses the same domain and native family Style. */
export function circularLoopPencil() {
  const base: PlaneCurvePoint = [-0.4, -0.2];
  return loopFamily(
    [
      [-0.4, 0.1],
      [-0.1, -0.2],
      [-0.15, 0.05],
      [-0.5, -0.25],
    ].map((c) => circularBasedLoop(c as [number, number], base)),
    base,
  );
}
export function buildCoverLoopFigure(renderOptions: FigureRenderOptions = {}) {
  return diagram({
    sub: coverLoopFan(),
    sty: planeLoopFamilyStyle({
      interactive: renderOptions.interactive
        ? {
            jitter: 3,
            maxDistance: 5,
            ...(typeof renderOptions.interactive === "object"
              ? renderOptions.interactive
              : {}),
          }
        : undefined,
    }),
    canvas: canvas(724, 724),
    variation: "elementary-topology-cover-loop-fan",
    ...renderOptions,
  });
}
export function buildCircularLoopPencilFigure(
  renderOptions: FigureRenderOptions = {},
) {
  return diagram({
    sub: circularLoopPencil(),
    sty: planeLoopFamilyStyle({
      units: 160,
      strokeWidth: 1.6,
      strokeColor: [0.08, 0.08, 0.08, 1],
      interactive: renderOptions.interactive
        ? {
            jitter: 3,
            maxDistance: 5,
            ...(typeof renderOptions.interactive === "object"
              ? renderOptions.interactive
              : {}),
          }
        : undefined,
    }),
    canvas: canvas(336, 336),
    variation: "elementary-topology-circle-loop-pencil",
    ...renderOptions,
  });
}
