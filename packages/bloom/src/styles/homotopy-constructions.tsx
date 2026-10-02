/** @jsxImportSource @penrose/bloom */

import type { Path } from "../core/types.js";
import {
  parabolicArcValue,
  pointSetTopology as topology,
  type PlaneCoordinates,
} from "../domains/point-set-topology.js";
import {
  homotopyCylinderView,
  homotopyFamilyStyle,
  type HomotopyFamilyStyleOptions,
} from "./homotopies.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

type XY = [number, number];
const INK: [number, number, number, number] = [0.08, 0.08, 0.08, 1];
const CLEAR: [number, number, number, number] = [0, 0, 0, 0];

/** A cubic representation of a mathematical parabolic arc, without raster/SVG passthrough. */
export function parabolicArcControls(data: {
  endpoints: readonly [PlaneCoordinates, PlaneCoordinates];
  height: number;
}) {
  const a = parabolicArcValue(data, 0),
    b = parabolicArcValue(data, 1),
    middle = parabolicArcValue(data, 0.5);
  const control: XY = [
    2 * middle[0] - (a[0] + b[0]) / 2,
    2 * middle[1] - (a[1] + b[1]) / 2,
  ];
  return [
    a,
    [
      a[0] + (2 * (control[0] - a[0])) / 3,
      a[1] + (2 * (control[1] - a[1])) / 3,
    ],
    [
      b[0] + (2 * (control[0] - b[0])) / 3,
      b[1] + (2 * (control[1] - b[1])) / 3,
    ],
    b,
  ] as const;
}
export function ParabolicArcView({
  data,
  units = 75,
  style = {},
  name = "parabolic-arc",
  strokeWidth = 0.6,
}: {
  data: {
    endpoints: readonly [PlaneCoordinates, PlaneCoordinates];
    height: number;
  };
  units?: number;
  style?: TopologyStyleOptions;
  name?: string;
  strokeWidth?: number;
}): Path {
  if (!(units > 0) || !Number.isFinite(units))
    throw new Error("An arc view needs finite positive units");
  const draw = topologyDrawing(style);
  const [a, c1, c2, b] = parabolicArcControls(data).map(([x, y]) => [
    units * x,
    units * y,
  ]);
  return (
    <path
      name={name}
      d={draw.data([
        ["M", ...a],
        ["C", ...c1, ...c2, ...b],
      ])}
      fill-color={CLEAR}
      stroke-color={INK}
      stroke-width={strokeWidth}
      aria-label={name}
    />
  ) as Path;
}

/** Disk images and constant slices share one mathematically continuous contract-and-slide family. */
export function contractAndSlideStyle(
  options: TopologyStyleOptions & {
    sourceCylinderOrder?: boolean;
    domainAnnotation?: string;
  } = {},
) {
  return topology.style((ctx) => {
    const H = ctx.entities(topology.ContractAndSlideHomotopy)[0];
    const endpoints =
      H && ctx.facts(topology.HomotopyBetween).find(([family]) => family === H);
    const mapping =
      H && ctx.facts(topology.MapBetween).find(([map]) => map === H);
    const factors =
      mapping &&
      ctx.facts(topology.ProductOf).find(([product]) => product === mapping[1]);
    const Y =
      factors &&
      ctx.entities(topology.ClosedDisk).find((disk) => disk === factors[1]);
    if (
      !H ||
      !endpoints ||
      !mapping ||
      !Y ||
      H.breakpoint !== 0.5 ||
      !ctx.test(topology.IdentityOn, endpoints[1], Y)
    )
      throw new Error(
        "The illustrated family needs a disk identity at1 and an interior contraction breakpoint",
      );
    const target = ctx
      .facts(topology.ConstantTo)
      .find(([map]) => map === endpoints[2])?.[1];
    const targetPoint =
      target &&
      ctx.entities(topology.CoordinatePoint).find((p) => p === target);
    if (
      !targetPoint ||
      targetPoint.coordinates.some((v, i) => v !== H.target[i]) ||
      H.center.some((v, i) => v !== Y.center[i]) ||
      targetPoint.coordinates[1] !== Y.center[1]
    )
      throw new Error(
        "The family must slide to the named horizontal constant point",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "10px",
    });
    const cx = -130,
      bottom = -60,
      height = 122,
      radius = 43,
      centerX = 54,
      imageRadius = 52;
    const slices = ctx
      .facts(topology.SliceMapAt)
      .filter(([, family]) => family === H);
    for (const t of [0, 0.5, 0.75, 1])
      if (!slices.some(([, , time]) => time.coordinate === t))
        throw new Error(
          "The construction needs both endpoints and the two named intermediate slices",
        );
    homotopyCylinderView({
      name: "contract-and-slide.domain",
      center: [cx, bottom],
      radius,
      height,
      style: options,
      slices: [
        { position: 1, hatch: true, mark: true },
        { position: 0.5, mark: true },
        { position: options.sourceCylinderOrder ? 0.27 : 0.75, mark: true },
        { position: 0, hatch: true, mark: true },
      ],
    });
    for (const [t, position, label] of [
      [1, 1, "1"],
      [0.5, 0.5, "\\tfrac12"],
      [0.75, options.sourceCylinderOrder ? 0.27 : 0.75, "\\tfrac34"],
      [0, 0, "0"],
    ] as const) {
      const slice = slices.find(
        ([, , parameter]) => parameter.coordinate === t,
      )!;
      const image = ctx
        .facts(topology.ImageOf)
        .find(([, map, source]) => map === slice[0] && source === Y)?.[0];
      if (!image)
        throw new Error("Each named slice needs its mathematical image");
      draw.label(label, [cx + radius + 6, bottom + height * position]);
    }
    draw.label(options.domainAnnotation ?? mapping[1].label, [cx, bottom - 17]);
    const disks = ctx
      .facts(topology.ImageOf)
      .filter(
        ([, map, source]) =>
          source === Y && slices.some(([slice]) => slice === map),
      )
      .flatMap(([set]) =>
        ctx.entities(topology.ClosedDisk).filter((disk) => disk === set),
      )
      .sort((a, b) => b.radius - a.radius);
    for (const disk of disks)
      <circle
        center={draw.xy([centerX, 0])}
        r={((imageRadius * disk.radius) / Y.radius) * draw.scale}
        fill-color={
          disk === Y ? options.regionColor ?? [0.95, 0.41, 0.12, 0.06] : CLEAR
        }
        stroke-color={INK}
        stroke-width={0.75}
        aria-label={"contract-and-slide image radius " + disk.radius}
      />;
    draw.line(
      "contract-and-slide.x-axis",
      [centerX - 69, 0],
      [centerX + 69, 0],
    );
    draw.line("contract-and-slide.y-axis", [centerX, -69], [centerX, 69]);
    const targetX =
      centerX +
      (imageRadius * (targetPoint.coordinates[0] - Y.center[0])) / Y.radius;
    const center = ctx
      .entities(topology.CoordinatePoint)
      .find((p) => p.coordinates.every((v, i) => v === H.center[i]))!;
    for (const [name, x] of [
      ["origin", centerX],
      ["quarter-point", (centerX + targetX) / 2],
      ["constant-point", targetX],
    ] as const)
      <circle
        name={"contract-and-slide." + name}
        center={draw.xy([x, 0])}
        r={1.8 * draw.scale}
        fill-color={INK}
        stroke-width={0}
        aria-label={name}
      />;
    const clearLabel = (text: string, at: XY, width: number) => {
      <rect
        center={draw.xy(at)}
        width={width * draw.scale}
        height={12 * draw.scale}
        fill-color={[1, 1, 1, 1]}
        stroke-width={0}
      />;
      draw.label(text, at);
    };
    clearLabel(center.label, [centerX - 12, -10], 29);
    clearLabel(targetPoint.label, [targetX + 15, -9], 31);
    for (const [timeLabel, anchor, y] of [
      ["\\tfrac12", centerX, 47],
      ["\\tfrac34", (centerX + targetX) / 2, 29],
      ["0", targetX, 12],
    ] as const) {
      draw.label("H(Y\\times\\{" + timeLabel + "\\})", [155, y]);
      const corner: XY = [anchor + 14, y];
      draw.line("contract-and-slide.leader-horizontal", [122, y], corner);
      draw.line("contract-and-slide.leader-diagonal", corner, [anchor, 1]);
    }
    draw.label("Y=H(Y\\times\\{1\\})", [centerX + 25, -62]);
    draw.line("contract-and-slide.map-arrow", [-69, 30], [-42, 30]);
    <polygon
      points={[
        [-42, 30],
        [-46, 32],
        [-46, 28],
      ].map(([x, y]) => draw.xy([x, y]))}
      fill-color={INK}
      stroke-width={0}
    />;
  });
}

/** Relative endpoint homotopies in the plane meet a winding obstruction after removing P. */
export function fixedEndpointArcsStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const relative = ctx.facts(topology.RelativeHomotopyOn)[0];
    const endpoints =
      relative &&
      ctx.facts(topology.HomotopyBetween).find(([H]) => H === relative[0]);
    const first =
        endpoints &&
        ctx.entities(topology.ParabolicArc).find((a) => a === endpoints[1]),
      last =
        endpoints &&
        ctx.entities(topology.ParabolicArc).find((a) => a === endpoints[2]);
    const deleted = ctx.facts(topology.DeletedPointFrom)[0];
    if (
      !relative ||
      !first ||
      !last ||
      !deleted ||
      first.endpoints.some((p, i) =>
        p.some((v, k) => v !== last.endpoints[i][k]),
      )
    )
      throw new Error(
        "A relative arc family needs shared endpoints and the removed interior point",
      );
    const restrictedFirst = ctx
        .facts(topology.CorestrictionOf)
        .find(
          ([, original, target]) => original === first && target === deleted[0],
        )?.[0],
      restrictedLast = ctx
        .facts(topology.CorestrictionOf)
        .find(
          ([, original, target]) => original === last && target === deleted[0],
        )?.[0];
    const tauY = ctx
      .facts(topology.TopologyOn)
      .find(([, set]) => set === deleted[0])?.[0];
    if (
      !restrictedFirst ||
      !restrictedLast ||
      !tauY ||
      !ctx.test(
        topology.NotHomotopicRelativeTo,
        restrictedFirst,
        restrictedLast,
        relative[1],
        tauY,
      )
    )
      throw new Error(
        "The puncture obstruction must keep the closed endpoint subset fixed",
      );
    const draw = topologyDrawing({
        ...options,
        fontSize: options.fontSize ?? "8.5px",
      }),
      units = 75;
    const world = ([x, y]: readonly [number, number]): XY => [
      units * x,
      units * y,
    ];
    const outer = parabolicArcControls(first).map(world),
      inner = parabolicArcControls(last).map(world);
    <path
      d={draw.data([
        ["M", ...outer[0]],
        ["C", ...outer[1], ...outer[2], ...outer[3]],
        ["C", ...inner[2], ...inner[1], ...inner[0]],
        ["Z"],
      ])}
      fill-color={options.regionColor ?? [0.95, 0.41, 0.12, 0.06]}
      stroke-width={0}
      aria-label="lens between the endpoint arcs"
    />;
    for (let i = 0; i <= 20; i++)
      <ParabolicArcView
        data={{
          endpoints: first.endpoints,
          height: last.height + ((first.height - last.height) * i) / 20,
        }}
        units={units}
        style={options}
        name={"relative-arcs.slice-" + i}
        strokeWidth={i === 0 || i === 20 ? 0.8 : 0.55}
      />;
    const p = world(deleted[2].coordinates);
    <circle
      center={draw.xy(p)}
      r={1.8 * draw.scale}
      fill-color={INK}
      stroke-width={0}
      aria-label="removed point P"
    />;
    draw.label(deleted[2].label, [p[0] - 8, p[1] - 1]);
    const pathEndpoints = ctx
      .facts(topology.PathEndpointsOf)
      .find(([path]) => path === first)!;
    draw.label(pathEndpoints[1].label, [-77, -17]);
    draw.label(pathEndpoints[2].label, [33, 58]);
    draw.label(first.label + "([0,1])", [-16, 53]);
    draw.label(last.label + "([0,1])", [12, -49]);
    const finish = world(first.endpoints[1]);
    draw.line(
      "relative-arcs.endpoint-leader",
      [finish[0] - 6, finish[1] + 25],
      [finish[0], finish[1] + 1],
    );
  });
}

/** Pasting remains a reusable mathematical family view with source label overrides isolated in Style. */
export function pastedHomotopyStyle(options: HomotopyFamilyStyleOptions = {}) {
  const drawing = homotopyFamilyStyle({
    sliceLabels: "parameters",
    imageLabels: "leaders",
    targetHorizontalScale: 0.88,
    targetVerticalScale: 1.08,
    cylinderRadius: 44,
    cylinderHeight: 116,
    cylinderBottom: -66,
    cylinderSection: "abstract",
    fontSize: "12px",
    familyArrowHeight: 0,
    imageLabelOverrides: { zero: "k(X)=H(X\\times\\{1\\})" },
    ...options,
  });
  return topology.style((ctx) => {
    const pasting = ctx.facts(topology.HomotopyPastedFrom)[0];
    const total =
      pasting &&
      ctx.facts(topology.HomotopyBetween).find(([H]) => H === pasting[0]);
    const upper =
      pasting &&
      ctx.facts(topology.HomotopyBetween).find(([H]) => H === pasting[1]);
    const lower =
      pasting &&
      ctx.facts(topology.HomotopyBetween).find(([H]) => H === pasting[2]);
    if (
      !pasting ||
      !total ||
      !upper ||
      !lower ||
      upper[2] !== lower[1] ||
      total[1] !== upper[1] ||
      total[2] !== lower[2]
    )
      throw new Error(
        "Pasted homotopies must agree on their common map and preserve the outer endpoint maps",
      );
    drawing.apply(ctx);
  });
}

/** The boundary extension view reuses endpoint fibers and the same native homotopy cylinder. */
export function endpointExtensionStyle(
  options: HomotopyFamilyStyleOptions = {},
) {
  const drawing = homotopyFamilyStyle({
    showMiddle: false,
    endpointHatching: true,
    imageLabels: "none",
    arrowMode: "endpoints",
    endpointFiberLabels: "on-fibers",
    targetHorizontalScale: 0.72,
    targetVerticalScale: 0.92,
    cylinderRadius: 42,
    cylinderHeight: 90,
    cylinderBottom: -50,
    fontSize: "11.5px",
    ...options,
  });
  return topology.style((ctx) => {
    const extension = ctx.facts(topology.ExtensionOf)[0];
    const boundary =
      extension &&
      ctx.entities(topology.EndpointFibers).find((set) => set === extension[2]);
    const domain =
      extension &&
      ctx.facts(topology.MapBetween).find(([map]) => map === extension[0])?.[1];
    if (
      !extension ||
      !boundary ||
      !domain ||
      !ctx.entities(topology.ProductSet).some((p) => p === domain) ||
      !ctx
        .facts(topology.EndpointFibersOf)
        .some(([set, product]) => set === boundary && product === domain)
    )
      throw new Error(
        "The extended map must agree with the endpoint-boundary map on both fibers",
      );
    drawing.apply(ctx);
  });
}
