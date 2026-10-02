/** @jsxImportSource @penrose/bloom */
import type { Ellipse, Path } from "../core/types.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";
type Command = [string, ...number[]];
function pageChart(
  draw: ReturnType<typeof topologyDrawing>,
  center: readonly [number, number],
) {
  const xy = (x: number, y: number): [number, number] => [
    x - center[0],
    center[1] - y,
  ];
  const data = (commands: Command[]) =>
    draw.data(
      commands.map(([cmd, ...p]) => [
        cmd,
        ...p.flatMap((_, i) => (i % 2 ? [] : xy(p[i], p[i + 1]))),
      ]),
    );
  return { xy, data };
}
/** Source-like sphere chart of a based contraction in the complement of an omitted pole. */
export function sphereLoopContractionStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const [family, loop, sphere, pole, base] =
      ctx.facts(topology.SphereLoopContractionOf)[0] ?? [];
    if (
      !family ||
      !loop ||
      !sphere ||
      !pole ||
      !base ||
      !ctx.test(topology.SphereLoopAvoids, loop, pole, sphere)
    )
      throw new Error(
        "A sphere loop contraction requires an omitted pole outside the loop image",
      );
    const based = ctx
      .facts(topology.LoopBasedAt)
      .find(([a, b]) => a === loop && b === base);
    if (
      !based ||
      !ctx.test(topology.NullHomotopic, loop, based[2], base) ||
      !ctx.test(topology.NotContractible, based[2])
    )
      throw new Error(
        "Distinguish a nullhomotopic loop from a contractible sphere",
      );
    const draw = topologyDrawing({
        ...options,
        fontSize: options.fontSize ?? "13px",
      }),
      chart = pageChart(draw, [588, 658]);
    <ellipse
      name="sphere.surface"
      center={draw.xy([0, 0])}
      rx={110 * draw.scale}
      ry={115 * draw.scale}
      fill-color={options.regionColor ?? [0.95, 0.41, 0.12, 0.045]}
      stroke-color={[0.08, 0.08, 0.08, 1]}
      stroke-width={1.05}
    />;
    const paths: Path[] = [];
    const curve = (name: string, commands: Command[], dash = false) =>
      paths.push(
        (
          <path
            name={name}
            d={chart.data(commands)}
            fill-color={[0, 0, 0, 0]}
            stroke-color={[0.08, 0.08, 0.08, 1]}
            stroke-width={1.1}
            stroke-dasharray={dash ? "5 4" : ""}
          />
        ) as Path,
      );
    curve(
      "sphere.equator-back",
      [
        ["M", 478, 659],
        ["C", 480, 618, 690, 618, 697, 659],
      ],
      true,
    );
    curve("sphere.equator-front", [
      ["M", 478, 659],
      ["C", 480, 701, 690, 699, 697, 659],
    ]);
    for (const [i, commands] of (
      [
        [
          ["M", 535, 697],
          ["C", 540, 676, 546, 577, 524, 576],
          ["C", 488, 575, 478, 673, 535, 697],
        ],
        [
          ["M", 535, 697],
          ["C", 516, 673, 535, 618, 515, 622],
          ["C", 492, 623, 494, 689, 535, 697],
        ],
        [
          ["M", 535, 697],
          ["C", 521, 680, 511, 649, 516, 654],
          ["C", 526, 651, 523, 690, 535, 697],
        ],
        [
          ["M", 535, 697],
          ["C", 556, 578, 619, 549, 657, 569],
          ["C", 687, 585, 704, 636, 695, 646],
          ["C", 648, 597, 576, 651, 535, 697],
        ],
        [
          ["M", 535, 697],
          ["C", 570, 662, 678, 636, 689, 681],
          ["C", 700, 735, 653, 768, 602, 767],
          ["C", 557, 762, 535, 730, 535, 697],
        ],
        [
          ["M", 535, 697],
          ["C", 570, 695, 653, 686, 666, 716],
          ["C", 682, 752, 652, 768, 607, 764],
          ["C", 566, 760, 543, 731, 535, 697],
        ],
        [
          ["M", 535, 697],
          ["C", 562, 705, 651, 700, 654, 728],
          ["C", 659, 758, 574, 763, 535, 697],
        ],
      ] as Command[][]
    ).entries())
      curve(`sphere.loop-slice-${i}`, commands);
    const mask = (
      <ellipse
        center={draw.xy([0, 0])}
        rx={110 * draw.scale}
        ry={115 * draw.scale}
        fill-color={[1, 1, 1, 1]}
        stroke-width={0}
      />
    ) as Ellipse;
    <g name="sphere.visible-loop-family" clip-path={mask}>
      {paths}
    </g>;
    draw.dot("sphere.basepoint", chart.xy(535, 697));
    draw.label("y_0", chart.xy(546, 727));
    draw.label("a", chart.xy(601, 716));
    draw.label("P", chart.xy(629, 741));
  });
}
/** A legible native torus schematic, shared by two factor-generator loops. */
export function torusGeneratorStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const loops = ctx.entities(topology.TorusFactorLoop),
      facts = ctx.facts(topology.FactorLoopOn);
    if (
      loops.length !== 2 ||
      new Set(loops.map((l) => l.factor)).size !== 2 ||
      loops.some((l) => l.winding !== 1) ||
      facts.length !== 2 ||
      facts[0][1] !== facts[1][1] ||
      facts[0][2] !== facts[1][2]
    )
      throw new Error(
        "The torus view needs both unit-winding factor loops at their common product base point",
      );
    const draw = topologyDrawing({
        ...options,
        fontSize: options.fontSize ?? "13px",
      }),
      ink: [number, number, number, number] = [0.08, 0.08, 0.08, 1];
    <ellipse
      name="torus.surface"
      center={draw.xy([0, -2])}
      rx={98 * draw.scale}
      ry={60 * draw.scale}
      fill-color={options.regionColor ?? [0.95, 0.41, 0.12, 0.055]}
      stroke-color={ink}
      stroke-width={1.05}
    />;
    <ellipse
      name="torus.hole"
      center={draw.xy([0, -3])}
      rx={53 * draw.scale}
      ry={20 * draw.scale}
      fill-color={[1, 1, 1, 1]}
      stroke-color={ink}
      stroke-width={1.05}
    />;
    const path = (name: string, commands: Command[], dash = false) => (
      <path
        name={name}
        d={draw.data(commands)}
        fill-color={[0, 0, 0, 0]}
        stroke-color={ink}
        stroke-width={1.1}
        stroke-dasharray={dash ? "4 3" : ""}
      />
    );
    path("torus.generator-b-visible", [
      ["M", -59, 15],
      ["C", -59, 39, 59, 39, 59, 15],
    ]);
    path(
      "torus.generator-b-hidden",
      [
        ["M", -59, 15],
        ["C", -62, -48, 62, -48, 59, 15],
      ],
      true,
    );
    path(
      "torus.generator-a-hidden",
      [
        ["M", 0, 33],
        ["C", -5, 37, -5, 49, 0, 58],
      ],
      true,
    );
    path(
      "torus.generator-a-visible",
      [
        ["M", 0, 58],
        ["C", 6, 47, 5, 37, 0, 33],
      ],
      false,
    );
    draw.dot("torus.basepoint", [0, 33]);
    draw.line("torus.arrow-a-left", [-3, 41], [0, 46]);
    draw.line("torus.arrow-a-right", [0, 46], [3, 41]);
    draw.line("torus.arrow-b-left", [-1, -33], [3, -35]);
    draw.line("torus.arrow-b-right", [3, -35], [-1, -37]);
    draw.label("a", [10, 46]);
    draw.label("b", [3, -48]);
    draw.label("y_0", [10, 19]);
  });
}
/** A based loop transported along j, with the reverse traversal occupying the same arc image. */
export function basepointChangeStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const [result, loop, path] =
      ctx.facts(topology.BasepointConjugateOf)[0] ?? [];
    const endpoints =
        path && ctx.facts(topology.PathEndpointsOf).find(([p]) => p === path),
      change =
        path &&
        ctx.facts(topology.BasepointChangeAlong).find(([, p]) => p === path);
    if (
      !result ||
      !loop ||
      !path ||
      !endpoints ||
      !change ||
      !ctx.facts(topology.ReversedPathOf).some(([, p]) => p === path)
    )
      throw new Error(
        "A basepoint change requires an arc j and its reverse, not an inverse based loop",
      );
    const draw = topologyDrawing({
        ...options,
        fontSize: options.fontSize ?? "13px",
      }),
      chart = pageChart(draw, [615.5, 810.5]);
    <path
      name="basepoint.loop-a"
      d={chart.data([
        ["M", 619, 822],
        ["C", 628, 786, 619, 740, 591, 725],
        ["C", 551, 705, 513, 748, 505, 780],
        ["C", 492, 824, 511, 866, 548, 856],
        ["C", 578, 847, 610, 844, 619, 822],
      ])}
      fill-color={[0, 0, 0, 0]}
      stroke-color={[0.08, 0.08, 0.08, 1]}
      stroke-width={1.15}
    />;
    draw.line("basepoint.arc-j", chart.xy(619, 821), chart.xy(724, 885));
    draw.line("basepoint.arc-reverse", chart.xy(619, 825), chart.xy(724, 889));
    for (const [name, x, y, sign] of [
      ["j", 678, 858, -1],
      ["reverse", 679, 865, 1],
    ] as const) {
      const tip = chart.xy(x, y);
      draw.line(
        `basepoint.arrow-${name}-a`,
        tip,
        chart.xy(x - sign * 6, y - sign * 1),
      );
      draw.line(
        `basepoint.arrow-${name}-b`,
        tip,
        chart.xy(x - sign * 3, y - sign * 6),
      );
    }
    draw.line("basepoint.loop-arrow-a", chart.xy(515, 766), chart.xy(512, 756));
    draw.line("basepoint.loop-arrow-b", chart.xy(515, 766), chart.xy(523, 758));
    draw.dot("basepoint.y0", chart.xy(619, 822));
    draw.dot("basepoint.y1", chart.xy(724, 887));
    draw.label("a", chart.xy(528, 760));
    draw.label("y_0", chart.xy(625, 841));
    draw.label("y_1", chart.xy(725, 899));
    draw.label("j", chart.xy(679, 843));
    draw.label("j^{-1}", chart.xy(671, 871));
  });
}
