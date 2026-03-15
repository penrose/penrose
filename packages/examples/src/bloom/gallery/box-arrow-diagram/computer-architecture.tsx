/** @jsxImportSource @penrose/bloom */
// Gallery translation of: box-arrow-diagram/computer-architecture

import type { Diagram, Line, Text as TextShape } from "@penrose/bloom";
import {
  DiagramBuilder,
  add,
  canvas,
  constraints,
  objectives,
  ops,
  shapeDistance,
  shapeDistanceLines,
  signedDistance,
} from "@penrose/bloom";

const SEED = "ButtermelonPenguin8290";

type ComponentRecord = {
  id: string;
  label: string;
  bus?: boolean;
};

const components: ComponentRecord[] = [
  { id: "BUS", label: "Data Bus", bus: true },
  { id: "MAR", label: "Memory Address Register" },
  { id: "RAM", label: "Random Access Memory" },
  { id: "IR", label: "Instruction Register" },
  { id: "CS", label: "Control Signals" },
  { id: "PC", label: "Program Counter" },
  { id: "RA", label: "Register A" },
  { id: "RB", label: "Register B" },
  { id: "ALU", label: "Arithmetic Logic Unit" },
  { id: "RF", label: "Flags Register" },
  { id: "RO", label: "Output Register" },
  { id: "D", label: "7-Segment Display" },
  { id: "LCD", label: "LCD Display" },
  { id: "RSP", label: "Stack Pointer Register" },
  { id: "IN", label: "Input Module" },
  { id: "C", label: "Clock Module" },
];

const signals = [
  ["BUS", "MAR"],
  ["BUS", "RAM"],
  ["RAM", "BUS"],
  ["MAR", "RAM"],
  ["BUS", "IR"],
  ["IR", "BUS"],
  ["IR", "CS"],
  ["BUS", "PC"],
  ["PC", "BUS"],
  ["BUS", "RA"],
  ["RA", "BUS"],
  ["BUS", "RB"],
  ["RB", "ALU"],
  ["RA", "ALU"],
  ["ALU", "BUS"],
  ["ALU", "RF"],
  ["BUS", "RO"],
  ["RO", "D"],
  ["BUS", "LCD"],
  ["BUS", "RSP"],
  ["RSP", "BUS"],
  ["IN", "BUS"],
] as const satisfies readonly (readonly [string, string])[];

export const buildDiagram = async (variation = SEED): Promise<Diagram> => {
  const { build, ensure, encourage, layer } = new DiagramBuilder(
    canvas(1000, 1000),
    variation,
  );

  const instances = Object.fromEntries(
    components.map((component) => {
      const text = (<text string={component.label} />) as TextShape;
      const icon = component.bus ? (
        <rect
          width={add(text.width, 40)}
          height={400}
          fill-color={[1, 1, 1, 1]}
          stroke-color={[0, 0, 0, 1]}
          stroke-width={2}
        />
      ) : (
        <rect
          width={add(text.width, 30)}
          height={add(text.height, 30)}
          fill-color={[1, 1, 1, 1]}
          stroke-color={[0, 0, 0, 1]}
          stroke-width={2}
        />
      );
      layer(icon, text);
      return [component.id, { ...component, icon, text }];
    }),
  ) as Record<string, ComponentRecord & { icon: any; text: any }>;

  for (const a of Object.values(instances)) {
    for (const b of Object.values(instances)) {
      ensure(constraints.disjoint(a.icon, b.icon, 50));
    }
  }

  const signalShapes: Line[] = signals.map(([fromId, toId]) => {
    const from = instances[fromId];
    const to = instances[toId];
    const icon = (
      <line stroke-color={[0, 0, 0, 1]} end-arrowhead={"straight"} />
    ) as Line;

    ensure(constraints.equal(signedDistance(from.icon, icon.start), 5));
    ensure(constraints.equal(signedDistance(to.icon, icon.end), 5));
    encourage(objectives.lessThan(shapeDistance(from.icon, to.icon), 60));
    encourage(objectives.nearVec(icon.start, from.icon.center, 0), 50);
    encourage(objectives.nearVec(icon.end, to.icon.center, 0), 50);
    encourage(objectives.minimal(ops.vnorm(ops.vsub(icon.end, icon.start))));

    layer(icon, from.icon);
    layer(icon, to.icon);

    return icon;
  });

  for (let i = 0; i < signalShapes.length; i += 1) {
    for (let j = i + 1; j < signalShapes.length; j += 1) {
      ensure(
        constraints.greaterThan(
          shapeDistanceLines(
            signalShapes[i].start,
            signalShapes[i].end,
            signalShapes[j].start,
            signalShapes[j].end,
          ),
          0,
        ),
      );
    }
  }

  return await build();
};

export default async (variation = SEED): Promise<string> => {
  const diagram = await buildDiagram(variation);
  const { svg } = await diagram.render();
  diagram.discard();
  return svg.outerHTML;
};
