import { diagram, type FigureRenderOptions } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { trigonometricLoopValue } from "../domains/loop-operations.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import { inverseCancellationStyle } from "../styles/loop-retracing.js";

/** Proposition4: any loop followed by its parameter inverse contracts relative to its basepoint. */
export function inverseCancellationSubstance(
  loopFamily: "circle" | "figure-eight" = "circle",
  chosenTime?: number,
) {
  if (loopFamily !== "circle" && loopFamily !== "figure-eight")
    throw new Error("Choose the circle or figure-eight loop family");
  if (
    chosenTime !== undefined &&
    (!Number.isFinite(chosenTime) || chosenTime < 0 || chosenTime > 1)
  )
    throw new Error("Choose a finite homotopy time in [0,1]");
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
  const endpoints = s.EndpointPair({ endpoints: [0, 1], label: "\\{0,1\\}" });
  const square = s.ProductSet({ label: "[0,1]\\times[0,1]" });
  const tauI = s.Topology({ label: "\\tau_I" }),
    tauSquare = s.Topology({ label: "\\tau_{I\\times I}" });
  s.TopologyOn(tauY, Y);
  s.TopologyOn(tauI, I);
  s.TopologyOn(tauSquare, square);
  s.Subset(endpoints, I);
  s.ClosedIn(endpoints, tauI);
  s.ProductOf(square, I, I);
  s.ProductTopologyOf(tauSquare, tauI, tauI);
  const a = s.TrigonometricLoop(
    loopFamily === "circle"
      ? { constant: [0, 1], cosine: [[0, -1]], sine: [[1, 0]], label: "a" }
      : {
          constant: [0, 0],
          cosine: [],
          sine: [
            [1, 0],
            [0, 0.65],
          ],
          label: "a",
        },
  );
  const y0 = s.CoordinatePoint({
    coordinates: trigonometricLoopValue(a, 0),
    label: "y_0",
  });
  s.Member(y0, Y);
  const inverse = s.InverseLoop({ label: "a^{-1}", formula: "a⁻¹(r)=a(1−r)" });
  const product = s.LoopConcatenation({ label: "a\\mathbin{\\#}a^{-1}" });
  const k = s.ConstantLoop({ label: "k" });
  s.LoopInverseOf(inverse, a);
  s.LoopProductOf(product, a, inverse);
  s.ConstantLoopAt(k, y0, tauY);
  const G = s.FundamentalGroup({ label: "\\pi_1(Y,y_0)" });
  s.FundamentalGroupOf(G, tauY, y0);
  const classes = [a, inverse, product, k].map((loop) => {
    s.LoopBasedAt(loop, y0, tauY);
    s.MapBetween(loop, I, Y);
    s.ContinuousMap(loop, tauI, tauY);
    s.PathEndpointsOf(loop, y0, y0);
    const cls = s.LoopHomotopyClass({ label: "|" + loop.label + "|" });
    s.LoopClassOf(cls, loop, tauY, y0);
    s.ClassInFundamentalGroup(cls, G);
    return cls;
  });
  s.InverseElement(classes[1], classes[0], G);
  s.LoopClassProductOf(classes[2], classes[0], classes[1], G);
  s.IdentityElement(classes[3], G);
  s.TopologicalEqualSets(classes[2], classes[3]);
  s.NullHomotopic(product, tauY, y0);
  const H = s.Homotopy({ label: "H", formula: "H(r,s)=a(2 min(r,1−r)(1−s))" });
  s.MapBetween(H, square, Y);
  s.ContinuousMap(H, tauSquare, tauY);
  s.HomotopyBetween(H, k, product);
  s.RelativeHomotopyOn(H, endpoints);
  s.RelativelyHomotopic(k, product, endpoints, tauY);
  for (const time of chosenTime === undefined
    ? [0, 1 / 3, 2 / 3, 1]
    : [chosenTime]) {
    const parameter = s.RealPoint({
      coordinate: time,
      label:
        time === 0 || time === 1
          ? String(time)
          : time === 1 / 3
          ? "\\tfrac13"
          : time === 2 / 3
          ? "\\tfrac23"
          : String(Number(time.toPrecision(4))),
    });
    s.Member(parameter, I);
    const slice =
      time === 0
        ? product
        : time === 1
        ? k
        : s.Loop({
            label: "H_{" + parameter.label + "}",
            formula: "r↦a(2 min(r,1−r)(1−" + time + "))",
          });
    if (time !== 0 && time !== 1) {
      s.LoopBasedAt(slice, y0, tauY);
      s.MapBetween(slice, I, Y);
      s.ContinuousMap(slice, tauI, tauY);
      s.PathEndpointsOf(slice, y0, y0);
    }
    s.SliceMapAt(slice, H, parameter);
    const image = s.Set({ label: slice.label + "([0,1])" });
    s.ImageOf(image, slice, I);
    s.Subset(image, Y);
    s.Member(y0, image);
  }
  return s.make();
}
export const buildInverseCancellationFigure = (
  loopFamily: "circle" | "figure-eight" = "circle",
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: inverseCancellationSubstance(loopFamily),
    sty: inverseCancellationStyle(),
    canvas: canvas(512, 164),
    variation: "retracing-" + loopFamily,
    ...renderOptions,
  });

/** A genuine slice H(-,time); the reference loop and chart stay fixed across frames. */
export const buildInverseCancellationFrameFigure = (
  loopFamily: "circle" | "figure-eight",
  time: number,
  options: FigureRenderOptions = {},
) =>
  diagram({
    sub: inverseCancellationSubstance(loopFamily, time),
    sty: inverseCancellationStyle({ singleSlice: true }),
    canvas: canvas(256, 176),
    variation: "retracing-frame-" + loopFamily,
    ...options,
  });
