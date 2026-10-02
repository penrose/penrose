/** @jsxImportSource @penrose/bloom */

import type { Ellipse, PathData } from "../core/types.js";
import {
  radialContractionValue,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";
import { hatchedTopologyDisk } from "./topology-bases.js";

type XY = [number, number];
type Command = [string, ...number[]];
const INK: [number, number, number, number] = [0.08, 0.08, 0.08, 1];
const CLEAR: [number, number, number, number] = [0, 0, 0, 0];

export interface HomotopyCylinderOptions {
  name?: string;
  center: XY;
  radius: number;
  height: number;
  slices: readonly { position: number; hatch?: boolean; mark?: boolean }[];
  style?: TopologyStyleOptions;
  sectionProfile?: "ellipse" | "abstract";
}

/** Reusable native domain fibers; position is a view coordinate, not a mathematical parameter. */
export function homotopyCylinderView({
  name = "homotopy-cylinder",
  center: [cx, bottom],
  radius,
  height,
  slices,
  style = {},
  sectionProfile = "ellipse",
}: HomotopyCylinderOptions) {
  if (
    !(radius > 0 && height > 0) ||
    ![cx, bottom, radius, height].every(Number.isFinite) ||
    slices.some(
      (s) => !Number.isFinite(s.position) || s.position < 0 || s.position > 1,
    )
  )
    throw new Error(
      "Cylinder fibers need finite positive dimensions and normalized view positions",
    );
  const draw = topologyDrawing(style);
  draw.line(
    name + ".left-side",
    [cx - radius, bottom],
    [cx - radius, bottom + height],
  );
  draw.line(
    name + ".right-side",
    [cx + radius, bottom],
    [cx + radius, bottom + height],
  );
  for (const [i, slice] of slices.entries()) {
    const y = bottom + height * slice.position;
    if (slice.hatch)
      hatchedHomotopyEllipse(
        name + ".hatched-slice-" + i,
        [cx, y],
        [radius, 8],
        style,
      );
    else if (slice.position === 1 && sectionProfile === "abstract")
      <path
        d={draw.data([
          ["M", cx - radius, y],
          [
            "C",
            cx - radius,
            y + 17,
            cx - radius * 0.35,
            y + 17,
            cx + radius * 0.2,
            y + 11,
          ],
          [
            "C",
            cx + radius * 0.7,
            y + 7,
            cx + radius * 0.85,
            y + 19,
            cx + radius,
            y,
          ],
          ["C", cx + radius, y - 14, cx - radius, y - 14, cx - radius, y],
          ["Z"],
        ])}
        fill-color={[1, 1, 1, 1]}
        stroke-color={INK}
        stroke-width={0.8}
        aria-label="homotopy top domain slice"
      />;
    else if (slice.position === 1)
      <ellipse
        center={draw.xy([cx, y])}
        rx={radius * draw.scale}
        ry={8 * draw.scale}
        fill-color={[1, 1, 1, 1]}
        stroke-color={INK}
        stroke-width={0.8}
        aria-label="homotopy top domain slice"
      />;
    else {
      <path
        d={draw.data([
          ["M", cx - radius, y],
          ["C", cx - radius, y + 11, cx + radius, y + 11, cx + radius, y],
        ])}
        fill-color={CLEAR}
        stroke-color={INK}
        stroke-width={0.8}
        stroke-dasharray="4 3"
        aria-label="homotopy hidden back slice"
      />;
      draw.outline(name + ".front-slice-" + i, [
        ["M", cx - radius, y],
        ["C", cx - radius, y - 11, cx + radius, y - 11, cx + radius, y],
      ]);
    }
    if (slice.mark) draw.dot(name + ".marked-slice-" + i, [cx + radius, y]);
  }
}

export interface HomotopyFamilyStyleOptions extends TopologyStyleOptions {
  showMiddle?: boolean;
  sliceLabels?: "fibers" | "parameters";
  imageLabels?: "inside" | "leaders" | "none";
  imageLabelOverrides?: { zero?: string; middle?: string; one?: string };
  endpointHatching?: boolean;
  arrowMode?: "family" | "endpoints";
  familyArrowHeight?: number;
  endpointFiberLabels?: "right" | "on-fibers";
  targetHorizontalScale?: number;
  targetVerticalScale?: number;
  cylinderRadius?: number;
  cylinderHeight?: number;
  cylinderBottom?: number;
  cylinderSection?: "ellipse" | "abstract";
}

function whiteLabel(
  draw: ReturnType<typeof topologyDrawing>,
  text: string,
  center: XY,
  width: number,
) {
  <rect
    center={draw.xy(center)}
    width={width * draw.scale}
    height={12 * draw.scale}
    fill-color={[1, 1, 1, 1]}
    stroke-width={0}
  />;
  return draw.label(text, center);
}

/** Native affine images of the same disk hatch primitive give projected contraction slices. */
export function hatchedHomotopyEllipse(
  name: string,
  center: XY,
  radii: XY,
  options: TopologyStyleOptions = {},
) {
  if (!radii.every((r) => Number.isFinite(r) && r > 0))
    throw new Error("A projected disk needs positive finite radii");
  const draw = topologyDrawing(options);
  const ellipse = (
    <ellipse
      name={name}
      center={draw.xy(center)}
      rx={radii[0] * draw.scale}
      ry={radii[1] * draw.scale}
      fill-color={options.regionColor ?? [0.95, 0.41, 0.12, 0.14]}
      stroke-color={INK}
      stroke-width={0.8}
      aria-label={name}
    />
  ) as Ellipse;
  const clip = (
    <ellipse
      center={draw.xy(center)}
      rx={radii[0] * draw.scale}
      ry={radii[1] * draw.scale}
      fill-color={[1, 1, 1, 1]}
      stroke-width={0}
    />
  ) as Ellipse;
  const stripeData: PathData = [];
  const extent = Math.hypot(...radii);
  const angle = Math.PI / 4;
  for (let offset = -extent; offset < extent; offset += 1.4) {
    const point = (t: number): XY =>
      draw.xy([
        center[0] - Math.sin(angle) * offset + Math.cos(angle) * t,
        center[1] + Math.cos(angle) * offset + Math.sin(angle) * t,
      ]);
    stripeData.push(
      { cmd: "M", contents: [{ tag: "CoordV", contents: point(-extent) }] },
      { cmd: "L", contents: [{ tag: "CoordV", contents: point(extent) }] },
    );
  }
  <g name={name + ".hatching"} clip-path={clip}>
    <path
      d={stripeData}
      fill-color={CLEAR}
      stroke-color={[0.1, 0.1, 0.1, 0.65]}
      stroke-width={0.4}
    />
  </g>;
  return ellipse;
}

/** The same mathematical disk view supports its contractibility and a specific radial map. */
export function diskContractionStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const relation = ctx.facts(topology.RadialContractionOf)[0];
    const Y = relation?.[1] ?? ctx.entities(topology.ClosedDisk)[0];
    const tau =
      Y && ctx.facts(topology.TopologyOn).find(([, set]) => set === Y)?.[0];
    if (!Y || !tau || !ctx.test(topology.Contractible, tau) || !(Y.radius > 0))
      throw new Error("A disk contraction requires a contractible closed disk");
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "10px",
    });
    const radius = relation ? 60 : 44;
    const at = (p: readonly [number, number]): XY => [
      ((p[0] - Y.center[0]) * radius) / Y.radius,
      ((p[1] - Y.center[1]) * radius) / Y.radius,
    ];
    hatchedTopologyDisk(
      "homotopy.disk",
      draw.xy([0, 0]),
      radius * draw.scale,
      [Math.PI / 4],
      options.regionColor,
      { spacing: 1.6, strokeWidth: 0.45, strokeOpacity: 0.75 },
    );
    <circle
      center={draw.xy([0, 0])}
      r={radius * draw.scale}
      fill-color={CLEAR}
      stroke-color={INK}
      stroke-width={0.8}
      aria-label="closed source disk"
    />;
    if (relation) {
      const [map, , parameter] = relation;
      const imageFact = ctx
        .facts(topology.ImageOf)
        .find(([, f, source]) => f === map && source === Y);
      const image =
        imageFact &&
        ctx.entities(topology.ClosedDisk).find((disk) => disk === imageFact[0]);
      const points = ctx
        .facts(topology.MapsTo)
        .find(([f, p]) => f === map && p.label === "(x,y)");
      const p =
        points &&
        ctx.entities(topology.CoordinatePoint).find((v) => v === points[1]);
      const q =
        points &&
        ctx.entities(topology.CoordinatePoint).find((v) => v === points[2]);
      if (
        !image ||
        !p ||
        !q ||
        parameter.coordinate !== map.factor ||
        map.center.some((v, i) => v !== Y.center[i]) ||
        image.radius !== Y.radius * map.factor ||
        image.center.some((v, i) => v !== Y.center[i]) ||
        radialContractionValue(p.coordinates, map.center, map.factor).some(
          (v, i) => Math.abs(v - q.coordinates[i]) > 1e-12,
        )
      )
        throw new Error(
          "The map, image disk and marked point must agree with the radial formula",
        );
      hatchedTopologyDisk(
        "homotopy.contracted-image",
        draw.xy([0, 0]),
        radius * map.factor * draw.scale,
        [Math.PI / 4, -Math.PI / 4],
        [0.95, 0.41, 0.12, 0.28],
        { spacing: 1.6, strokeWidth: 0.45, strokeOpacity: 0.75 },
      );
      <circle
        center={draw.xy([0, 0])}
        r={radius * map.factor * draw.scale}
        fill-color={CLEAR}
        stroke-color={INK}
        stroke-width={0.8}
        aria-label="concentric radial image disk"
      />;
      draw.line("disk-contraction.x-axis", [-88, 0], [90, 0]);
      draw.line("disk-contraction.y-axis", [0, -87], [0, 87]);
      draw.dot("disk-contraction.source-point", at(p.coordinates));
      draw.dot("disk-contraction.image-point", at(q.coordinates));
      const pa = at(p.coordinates),
        qa = at(q.coordinates);
      whiteLabel(draw, p.label, [pa[0] + 14, pa[1] + 1], 29);
      whiteLabel(draw, q.label, [qa[0] + 25, qa[1] + 2], 49);
      const boundary = ctx
        .facts(topology.MapsTo)
        .find(([f, v]) => f === map && v.label === "(1,0)");
      if (!boundary)
        throw new Error(
          "The diagram needs the marked unit-radius boundary image",
        );
      whiteLabel(draw, boundary[2].label, [radius * map.factor + 12, -8], 31);
      whiteLabel(draw, boundary[1].label, [radius + 16, -8], 28);
      draw.label("x", [95, 0]);
      draw.label("y", [0, 93]);
    } else {
      draw.line("disk-contraction.x-axis", [-73, 0], [73, 0]);
      draw.line("disk-contraction.y-axis", [0, -73], [0, 73]);
      draw.label("x", [76, 0]);
      draw.label("y", [0, 76]);
      draw.label(Y.label, [34, -44]);
    }
    const origin = ctx
      .entities(topology.CoordinatePoint)
      .find(
        (v) =>
          v.coordinates.every((c, i) => c === Y.center[i]) &&
          ctx.test(topology.Member, v, Y),
      );
    if (!origin)
      throw new Error("The included contraction center must be named");
    whiteLabel(draw, origin.label, [13, -8], 27);
  });
}

/** The graph of the radial family is displayed as a cone of disk images at parameter heights. */
export function radialHomotopyStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const family = ctx.entities(topology.RadialHomotopy)[0];
    const endpoints =
      family && ctx.facts(topology.HomotopyBetween).find(([H]) => H === family);
    const domain =
      family && ctx.facts(topology.MapBetween).find(([H]) => H === family)?.[1];
    const factors =
      domain &&
      ctx.facts(topology.ProductOf).find(([product]) => product === domain);
    const Y =
      factors &&
      ctx.entities(topology.ClosedDisk).find((disk) => disk === factors[1]);
    const I =
      factors &&
      ctx
        .entities(topology.ClosedInterval)
        .find((interval) => interval === factors[2]);
    if (
      !family ||
      !endpoints ||
      !Y ||
      !I ||
      I.a !== 0 ||
      I.b !== 1 ||
      !ctx.test(topology.IdentityOn, endpoints[1], Y)
    )
      throw new Error(
        "The radial cone needs its disk×[0,1] family and identity endpoint",
      );
    const center = ctx
      .facts(topology.ConstantTo)
      .find(([f]) => f === endpoints[2])?.[1];
    if (!center || center.label !== "(0,0)")
      throw new Error("The zero-time slice must contract to the named center");
    const slices = ctx
      .facts(topology.SliceMapAt)
      .filter(([, H]) => H === family)
      .sort((a, b) => b[2].coordinate - a[2].coordinate);
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "10px",
    });
    const x = -18,
      bottom = -63,
      height = 112,
      rx = 43,
      ry = 17;
    draw.line(
      "radial-homotopy.left-side",
      [x - rx, bottom + height],
      [x, bottom],
    );
    draw.line(
      "radial-homotopy.right-side",
      [x + rx, bottom + height],
      [x, bottom],
    );
    for (const [map, , parameter] of slices) {
      const r = parameter.coordinate;
      if (!(r >= 0 && r <= 1))
        throw new Error("A homotopy slice must lie in [0,1]");
      const image = ctx
        .facts(topology.ImageOf)
        .find(([, f, source]) => f === map && source === Y)?.[0];
      if (!image) throw new Error("Every slice must have a named image");
      const disk = ctx.entities(topology.ClosedDisk).find((d) => d === image);
      if (r > 0 && (!disk || Math.abs(disk.radius - r * Y.radius) > 1e-12))
        throw new Error(
          "A positive radial slice has radius r times the original radius",
        );
      if (r > 0)
        hatchedHomotopyEllipse(
          "radial-homotopy.slice-" + r,
          [x, bottom + r * height],
          [rx * r, ry * r],
          options,
        );
      if (parameter.label === "1" || parameter.label === "r") {
        draw.dot("radial-homotopy.center-" + parameter.label, [
          x,
          bottom + r * height,
        ]);
        if (r === 1)
          whiteLabel(draw, center.label, [x + 15, bottom + r * height], 26);
        else {
          const y = bottom + r * height;
          draw.line(
            "radial-homotopy.center-leader-left",
            [-36, y + 11],
            [-28, y + 11],
          );
          draw.line(
            "radial-homotopy.center-leader-right",
            [-28, y + 11],
            [x, y],
          );
          draw.label(center.label, [-52, y + 12]);
        }
        draw.label(r === 1 ? "(1,0)" : "(r,0)", [
          x + rx * r + 17,
          bottom + r * height,
        ]);
        draw.label(parameter.label, [x + rx * r + 39, bottom + r * height]);
      }
    }
    draw.line("radial-homotopy.center-axis", [x, bottom], [x, bottom + height]);
    draw.label(Y.label, [x + rx + 3, bottom + height + ry + 1]);
    draw.label(center.label, [x + 18, bottom - 7]);
    draw.label("0", [x + 38, bottom - 7]);
  });
}

/** An abstract cylinder and its named slice images share the general homotopy vocabulary. */
export function homotopyFamilyStyle(options: HomotopyFamilyStyleOptions = {}) {
  return topology.style((ctx) => {
    const family = ctx.entities(topology.Homotopy)[0];
    const endpoints =
      family && ctx.facts(topology.HomotopyBetween).find(([H]) => H === family);
    const mapping =
      family && ctx.facts(topology.MapBetween).find(([H]) => H === family);
    const factors =
      mapping &&
      ctx.facts(topology.ProductOf).find(([product]) => product === mapping[1]);
    const I =
      factors &&
      ctx.entities(topology.ClosedInterval).find((i) => i === factors[2]);
    if (
      !family ||
      !endpoints ||
      !mapping ||
      !factors ||
      !I ||
      I.a !== 0 ||
      I.b !== 1
    )
      throw new Error(
        "The homotopy requires an X×[0,1] domain and both endpoint maps",
      );
    const X = factors[1],
      Y = mapping[2];
    const slices = ctx
      .facts(topology.SliceMapAt)
      .filter(([, H]) => H === family);
    const lower = slices.find(([, , t]) => t.coordinate === 0),
      upper = slices.find(([, , t]) => t.coordinate === 1),
      middle = slices.find(([, , t]) => t.coordinate > 0 && t.coordinate < 1);
    const showMiddle = options.showMiddle ?? true;
    if (
      !lower ||
      !upper ||
      (showMiddle && !middle) ||
      lower[0] !== endpoints[2] ||
      upper[0] !== endpoints[1]
    )
      throw new Error("H(-,0)=g and H(-,1)=f must preserve endpoint order");
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "10px",
    });
    const cx = -74,
      radius = options.cylinderRadius ?? 39,
      bottom = options.cylinderBottom ?? -57,
      height = options.cylinderHeight ?? 98;
    const displaySlices = showMiddle ? [upper, middle!, lower] : [upper, lower];
    homotopyCylinderView({
      name: "homotopy-family",
      center: [cx, bottom],
      radius,
      height,
      slices: displaySlices.map(([, , t]) => ({
        position: t.coordinate,
        hatch:
          options.endpointHatching &&
          (t.coordinate === 0 || t.coordinate === 1),
      })),
      style: options,
      sectionProfile: options.cylinderSection,
    });
    const targetScale = options.targetHorizontalScale ?? 1;
    const targetVertical = options.targetVerticalScale ?? 1;
    if (
      ![targetScale, targetVertical].every((v) => v > 0 && Number.isFinite(v))
    )
      throw new Error("Target scales must be finite and positive");
    const targetAt = ([x, y]: XY): XY => [
      77 + targetScale * (x - 77),
      targetVertical * y,
    ];
    const targetPath = (name: string, commands: Command[]) => (
      <path
        name={name}
        d={draw.data(
          commands.map(([cmd, ...values]) => [
            cmd,
            ...values.map((v, i) =>
              i % 2 === 0 ? 77 + targetScale * (v - 77) : targetVertical * v,
            ),
          ]),
        )}
        fill-color={CLEAR}
        stroke-color={INK}
        stroke-width={1.4}
        aria-label={name}
      />
    );
    for (const slice of displaySlices) {
      const [map, , t] = slice;
      const y = bottom + height * t.coordinate;
      const singleton = ctx
        .facts(topology.SingletonOf)
        .find(([, p]) => p === t)?.[0];
      const sliceSet =
        singleton &&
        ctx
          .facts(topology.ProductOf)
          .find(([, a, b]) => a === X && b === singleton)?.[0];
      const image = ctx
        .facts(topology.ImageOf)
        .find(([, f, source]) => f === map && source === X)?.[0];
      if (
        !sliceSet ||
        !image ||
        !ctx.test(topology.Subset, sliceSet, mapping[1]) ||
        !ctx.test(topology.Subset, image, Y)
      )
        throw new Error(
          "Every parameter slice needs a domain fiber and image contained in Y",
        );
      draw.label(
        options.sliceLabels === "parameters"
          ? t.coordinate === 0.5
            ? "\\tfrac12"
            : String(t.coordinate)
          : sliceSet.label,
        options.endpointFiberLabels === "on-fibers"
          ? [cx, y + (t.coordinate === 1 ? 22 : 0)]
          : [cx + radius + (options.sliceLabels === "parameters" ? 5 : 24), y],
      );
      const labelAt: XY =
        t.coordinate === 1
          ? [83, 57]
          : t.coordinate === 0
          ? [72, -65]
          : [77, 17];
      const label =
        (t.coordinate === 1
          ? options.imageLabelOverrides?.one
          : t.coordinate === 0
          ? options.imageLabelOverrides?.zero
          : options.imageLabelOverrides?.middle) ?? image.label;
      if (options.imageLabels === "leaders") {
        const destination = targetAt(
          t.coordinate === 1
            ? [79, 44]
            : t.coordinate === 0
            ? [80, -45]
            : [77, -44 + 85 * t.coordinate],
        );
        const labelY = destination[1] + 17;
        draw.label(label, [176, labelY]);
        draw.line(
          "homotopy-family.image-leader",
          [124, labelY - 7],
          destination,
        );
        const dx = 124 - destination[0],
          dy = labelY - 7 - destination[1],
          length = Math.hypot(dx, dy),
          ux = dx / length,
          uy = dy / length;
        <polygon
          points={[
            destination,
            [
              destination[0] + 4 * ux - 1.5 * uy,
              destination[1] + 4 * uy + 1.5 * ux,
            ],
            [
              destination[0] + 4 * ux + 1.5 * uy,
              destination[1] + 4 * uy - 1.5 * ux,
            ],
          ].map((p) => draw.xy(p as XY))}
          fill-color={INK}
          stroke-width={0}
        />;
      } else if (options.imageLabels !== "none")
        draw.label(label, targetAt(labelAt));
    }
    draw.label(mapping[1].label, [cx, bottom - 17]);
    targetPath("homotopy-family.target", [
      ["M", 63, 66],
      ["C", 31, 65, 40, 26, 31, 9],
      ["C", 22, -8, 1, -37, 24, -53],
      ["C", 43, -74, 91, -86, 115, -57],
      ["C", 126, -36, 122, 16, 119, 45],
      ["C", 116, 68, 85, 76, 63, 66],
      ["Z"],
    ]);
    targetPath("homotopy-family.image-band", [
      ["M", 48, 42],
      ["C", 67, 51, 90, 46, 107, 41],
      ["C", 112, 19, 110, -27, 105, -44],
      ["C", 87, -51, 56, -39, 24, -48],
      ["C", 30, -23, 59, 4, 48, 42],
      ["Z"],
    ]);
    const middleY = middle ? -44 + 85 * middle[2].coordinate : 0;
    if (showMiddle)
      targetPath("homotopy-family.intermediate-image", [
        ["M", 43, middleY],
        ["C", 59, middleY + 10, 91, middleY - 5, 110, middleY + 4],
      ]);
    const arrow = (label: string, y: number) => {
      draw.line("homotopy-family.map-arrow", [-7, y], [19, y]);
      <polygon
        points={[
          [19, y],
          [15, y + 2],
          [15, y - 2],
        ].map(([x, y]) => draw.xy([x, y]))}
        fill-color={INK}
        stroke-width={0}
      />;
      draw.label(label, [5, y + 7]);
    };
    if (options.arrowMode === "endpoints") {
      arrow(endpoints[1].label, bottom + height);
      arrow(endpoints[2].label, bottom);
    } else arrow(family.label, options.familyArrowHeight ?? 61);
    draw.label(Y.label, targetAt([125, -57]));
  });
}
