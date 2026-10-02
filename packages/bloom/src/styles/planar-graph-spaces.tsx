/** @jsxImportSource @penrose/bloom */
import type {
  DiagramBuilder,
  InteractiveLayoutOptions,
} from "../core/builder.js";
import type { Line, Path, Rectangle } from "../core/types.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";
type Command = [string, ...number[]];

export interface PlanarGraphSpacesStyleOptions extends TopologyStyleOptions {
  /** Move each complete space without changing its contours or incidences. */
  interactive?: InteractiveLayoutOptions;
}

/** One native hit region translates all contours and connecting strokes together. */
export function draggablePlanarSpace(
  builder: DiagramBuilder,
  shapes: readonly (Path | Line)[],
  name: string,
  label: string,
  options: InteractiveLayoutOptions,
  scale = 1,
) {
  if (!(scale > 0) || !Number.isFinite(scale))
    throw new Error("A planar drag scale must be finite and positive");
  const coordinates = shapes.flatMap((shape) =>
    "d" in shape
      ? shape.d.flatMap((command) =>
          command.contents.flatMap((value) =>
            value.tag === "CoordV" ? [value.contents] : [],
          ),
        )
      : [shape.start, shape.end],
  );
  if (
    !coordinates.length ||
    coordinates.some((p) =>
      p.some((n) => typeof n !== "number" || !Number.isFinite(n)),
    )
  )
    throw new Error(
      "A planar space needs fixed finite coordinates before dragging",
    );
  const xs = coordinates.map((p) => p[0] as number),
    ys = coordinates.map((p) => p[1] as number),
    x0 = Math.min(...xs),
    x1 = Math.max(...xs),
    y0 = Math.min(...ys),
    y1 = Math.max(...ys);
  const handle = (
    <rect
      name={name}
      center={[(x0 + x1) / 2, (y0 + y1) / 2]}
      width={x1 - x0 + 4 * scale}
      height={y1 - y0 + 4 * scale}
      fill-color={[0, 0, 0, 0]}
      stroke-width={0}
      ensure-on-canvas={false}
      pointer-events="all"
      aria-label={`Move the complete ${label} space`}
    />
  ) as Rectangle;
  // Cubic control points can extend past the visible contour. The source
  // construction has three units of visible margin, so bound its translation
  // rather than letting a control-point bounding box pull it after release.
  shapes.forEach((shape) => {
    shape.ensureOnCanvas = false;
  });
  // The canonical source row has small margins; sampled movement stays mild.
  const maxDistance = Math.min(options.maxDistance ?? 3 * scale, 3 * scale);
  builder.draggableGroup(handle, shapes, {
    ...options,
    maxDistance,
    jitter: Math.min(options.jitter ?? 2 * scale, maxDistance),
  });
}
function ellipse(cx: number, cy: number, rx: number, ry: number): Command[] {
  const k = 0.5522847498;
  return [
    ["M", cx + rx, cy],
    ["C", cx + rx, cy + k * ry, cx + k * rx, cy + ry, cx, cy + ry],
    ["C", cx - k * rx, cy + ry, cx - rx, cy + k * ry, cx - rx, cy],
    ["C", cx - rx, cy - k * ry, cx - k * rx, cy - ry, cx, cy - ry],
    ["C", cx + k * rx, cy - ry, cx + rx, cy - k * ry, cx + rx, cy],
    ["Z"],
  ];
}
/** Fixed vector glyph contours keep source topology independent of host fonts. */
export function textbookGlyphOutline(symbol: string): Command[] {
  switch (symbol) {
    case "A":
      return [
        ["M", 0, 0],
        ["L", 0.36, 1],
        ["L", 0.64, 1],
        ["L", 1, 0],
        ["L", 0.76, 0],
        ["L", 0.66, 0.27],
        ["L", 0.34, 0.27],
        ["L", 0.24, 0],
        ["Z"],
        ["M", 0.41, 0.48],
        ["L", 0.59, 0.48],
        ["L", 0.5, 0.77],
        ["Z"],
      ];
    case "B":
      return [
        ["M", 0, 0],
        ["L", 0, 1],
        ["L", 0.56, 1],
        ["C", 1, 1, 1, 0.6, 0.73, 0.53],
        ["C", 1, 0.45, 1, 0, 0.56, 0],
        ["Z"],
        ["M", 0.23, 0.58],
        ["L", 0.55, 0.58],
        ["C", 0.75, 0.58, 0.75, 0.81, 0.55, 0.81],
        ["L", 0.23, 0.81],
        ["Z"],
        ["M", 0.23, 0.19],
        ["L", 0.57, 0.19],
        ["C", 0.78, 0.19, 0.78, 0.41, 0.57, 0.41],
        ["L", 0.23, 0.41],
        ["Z"],
      ];
    case "C":
      return [
        ["M", 0.98, 0.73],
        ["C", 0.83, 1.14, 0.12, 1.16, 0.04, 0.65],
        ["C", -0.1, 0.1, 0.69, -0.25, 0.99, 0.29],
        ["L", 0.74, 0.35],
        ["C", 0.53, 0.04, 0.2, 0.21, 0.26, 0.61],
        ["C", 0.29, 0.87, 0.62, 0.91, 0.73, 0.68],
        ["Z"],
      ];
    case "D":
      return [
        ["M", 0, 0],
        ["L", 0, 1],
        ["L", 0.45, 1],
        ["C", 1.18, 1, 1.18, 0, 0.45, 0],
        ["Z"],
        ["M", 0.24, 0.2],
        ["L", 0.45, 0.2],
        ["C", 0.89, 0.2, 0.89, 0.8, 0.45, 0.8],
        ["L", 0.24, 0.8],
        ["Z"],
      ];
    case "E":
      return [
        ["M", 0, 0],
        ["L", 0, 1],
        ["L", 0.88, 1],
        ["L", 0.88, 0.79],
        ["L", 0.24, 0.79],
        ["L", 0.24, 0.6],
        ["L", 0.82, 0.6],
        ["L", 0.82, 0.4],
        ["L", 0.24, 0.4],
        ["L", 0.24, 0.21],
        ["L", 0.91, 0.21],
        ["L", 0.91, 0],
        ["Z"],
      ];
    case "R":
      return [
        ["M", 0, 0],
        ["L", 0, 1],
        ["L", 0.55, 1],
        ["C", 1, 1, 1, 0.56, 0.65, 0.5],
        ["L", 1, 0],
        ["L", 0.73, 0],
        ["L", 0.41, 0.47],
        ["L", 0.24, 0.47],
        ["L", 0.24, 0],
        ["Z"],
        ["M", 0.24, 0.65],
        ["L", 0.53, 0.65],
        ["C", 0.76, 0.65, 0.76, 0.81, 0.53, 0.81],
        ["L", 0.24, 0.81],
        ["Z"],
      ];
    case "T":
      return [
        ["M", 0, 1],
        ["L", 1, 1],
        ["L", 1, 0.78],
        ["L", 0.62, 0.78],
        ["L", 0.62, 0],
        ["L", 0.38, 0],
        ["L", 0.38, 0.78],
        ["L", 0, 0.78],
        ["Z"],
      ];
    case "0":
      return [
        ...ellipse(0.5, 0.5, 0.48, 0.52),
        ...ellipse(0.5, 0.5, 0.23, 0.31),
      ];
    case "8":
      return [
        ["M", 0.5, 1.03],
        ["C", 1.05, 1.03, 1.1, 0.63, 0.78, 0.53],
        ["C", 1.15, 0.37, 1.05, -0.03, 0.5, -0.03],
        ["C", -0.05, -0.03, -0.15, 0.37, 0.22, 0.53],
        ["C", -0.1, 0.63, -0.05, 1.03, 0.5, 1.03],
        ["Z"],
        ...ellipse(0.5, 0.76, 0.2, 0.13),
        ...ellipse(0.5, 0.25, 0.23, 0.14),
      ];
    case "1":
      return [
        ["M", 0.16, 0.71],
        ["L", 0.16, 0.91],
        ["L", 0.43, 1],
        ["L", 0.66, 1],
        ["L", 0.66, 0],
        ["L", 0.4, 0],
        ["L", 0.4, 0.73],
        ["Z"],
      ];
    default:
      throw new Error("No source glyph contour is available for " + symbol);
  }
}
export function TextbookGlyphView({
  symbol,
  at = [0, 0],
  height = 26,
  width = 22,
  style = {},
  name = "glyph",
}: {
  symbol: string;
  at?: readonly [number, number];
  height?: number;
  width?: number;
  style?: TopologyStyleOptions;
  name?: string;
}): Path {
  if (
    !(height > 0 && width > 0) ||
    ![height, width, ...at].every(Number.isFinite)
  )
    throw new Error("Glyph dimensions must be finite and positive");
  const draw = topologyDrawing(style),
    d = draw.data(
      textbookGlyphOutline(symbol).map(([cmd, ...p]) => [
        cmd,
        ...p.flatMap((_, i) =>
          i % 2
            ? []
            : [at[0] + width * (p[i] - 0.5), at[1] + height * (p[i + 1] - 0.5)],
        ),
      ]),
    );
  return (
    <path
      name={name}
      d={d}
      fill-color={[0.08, 0.08, 0.08, 1]}
      stroke-width={0}
      fill-rule="evenodd"
    />
  ) as Path;
}
/** Eleven source exercise spaces: contours carry topology, rather than font-dependent labels. */
export function planarGraphSpacesStyle(
  options: PlanarGraphSpacesStyleOptions = {},
) {
  return topology.style((ctx) => {
    const spaces = ctx.entities(topology.PlanarDiagramSpace);
    if (
      spaces.length !== 11 ||
      new Set(spaces.filter((s) => s.kind === "glyph").map((s) => s.symbol))
        .size !== 9
    )
      throw new Error(
        "The source exercise view needs the eleven separate diagram spaces",
      );
    const draw = topologyDrawing(options),
      xy = (x: number, y: number): [number, number] => [x - 375, 770 - y],
      commands = (c: Command[]) =>
        c.map(
          ([cmd, ...p]): Command => [
            cmd,
            ...p.flatMap((_, i) => (i % 2 ? [] : xy(p[i], p[i + 1]))),
          ],
        );
    const movable = (symbol: string, shapes: readonly (Path | Line)[]) => {
      if (options.interactive)
        draggablePlanarSpace(
          ctx.builder,
          shapes,
          `exercise.space-${symbol}`,
          symbol,
          options.interactive,
          draw.scale,
        );
    };
    for (const [symbol, x] of [
      ["A", 227],
      ["B", 272],
      ["C", 319],
      ["D", 365],
      ["E", 411],
      ["R", 455],
      ["T", 497],
    ] as const)
      movable(symbol, [
        (
          <TextbookGlyphView
            symbol={symbol}
            at={xy(x, 729)}
            height={26}
            width={22}
            style={options}
            name={`exercise.glyph-${symbol}`}
          />
        ) as Path,
      ]);
    movable("8", [
      (
        <TextbookGlyphView
          symbol="8"
          at={xy(426, 807)}
          height={26}
          width={21}
          style={options}
          name="exercise.glyph-8"
        />
      ) as Path,
    ]);
    const compound: (Path | Line)[] = [];
    for (const [symbol, x, width] of [
      ["1", 476, 15],
      ["0", 492, 23],
      ["8", 516, 23],
    ] as const)
      compound.push(
        (
          <TextbookGlyphView
            symbol={symbol}
            at={xy(x, 807)}
            height={26}
            width={width}
            style={options}
            name={`exercise.108-${symbol}`}
          />
        ) as Path,
      );
    // The source compound 108 has touching ink, hence one connected space with three holes.
    compound.push(
      (
        <line
          name="exercise.108-top-join"
          start={draw.xy(xy(493, 796))}
          end={draw.xy(xy(514, 796))}
          stroke-color={[0.08, 0.08, 0.08, 1]}
          stroke-width={4.5}
        />
      ) as Line,
      (
        <line
          name="exercise.108-slanted-join"
          start={draw.xy(xy(478, 820))}
          end={draw.xy(xy(485, 801))}
          stroke-color={[0.08, 0.08, 0.08, 1]}
          stroke-width={3.7}
        />
      ) as Line,
    );
    movable("108", compound);
    const animal = draw.outline(
      "exercise.animal",
      commands([
        ["M", 226, 767],
        ["L", 216, 787],
        ["L", 230, 794],
        ["L", 239, 775],
        ["Z"],
        ["M", 239, 775],
        ["L", 249, 786],
        ["M", 249, 782],
        ["L", 281, 782],
        ["L", 281, 805],
        ["L", 249, 805],
        ["Z"],
        ["M", 255, 805],
        ["L", 250, 820],
        ["M", 256, 805],
        ["L", 256, 820],
        ["M", 277, 805],
        ["L", 271, 820],
        ["M", 280, 805],
        ["L", 278, 820],
        ["M", 281, 785],
        ["C", 288, 782, 293, 773, 293, 767],
      ]),
    );
    movable("animal", [animal]);
    const house = draw.outline(
      "exercise.house",
      commands([
        ["M", 316, 790],
        ["L", 342, 765],
        ["L", 368, 790],
        ["M", 323, 783.2692307692],
        ["L", 323, 820],
        ["L", 335, 820],
        ["L", 335, 801],
        ["L", 348, 801],
        ["L", 348, 820],
        ["L", 361, 820],
        ["L", 361, 783.2692307692],
        ["M", 350, 772.6923076923],
        ["L", 350, 766],
        ["L", 357, 766],
        ["L", 357, 779.4230769231],
      ]),
    );
    movable("house", [house]);
  });
}
