import { add } from "@penrose/core";
import { afterEach, expect, test } from "vitest";
import { DiagramBuilder, setActiveBuilder } from "./builder.js";
import type { PathData } from "./types.js";
import { canvas, toPenroseShape } from "./utils.js";

afterEach(() => setActiveBuilder(null));

test("path coordinates retain automatic differentiation and update through the renderer", async () => {
  const builder = new DiagramBuilder(canvas(100, 100), "variable-path");
  const x = builder.input({ name: "x", init: 10 });
  const d: PathData = [
    { cmd: "M", contents: [{ tag: "CoordV", contents: [x, 0] }] },
    {
      cmd: "C",
      contents: [
        { tag: "CoordV", contents: [add(x, 5), 10] },
        { tag: "CoordV", contents: [add(x, 10), 10] },
        { tag: "CoordV", contents: [add(x, 15), 0] },
      ],
    },
  ];
  const path = builder.path({ d, strokeWidth: 1 });
  expect(toPenroseShape(path)).toHaveProperty("d.tag", "PathDataV");
  const drawing = await builder.build();
  try {
    const { svg } = await drawing.render();
    const xml = new XMLSerializer().serializeToString(svg);
    const document = new DOMParser().parseFromString(xml, "image/svg+xml");
    expect(document.querySelector("parsererror")).toBeNull();
    const before = svg.querySelector("path")!;
    expect(before.getAttribute("d")).toContain("M 60 50");
    expect(before.getAttribute("d")).toContain("C 65 40 70 40 75 50");
    drawing.setInput("x", 20);
    const after = (await drawing.render()).svg.querySelector("path")!;
    expect(after.getAttribute("d")).toContain("M 70 50");
    expect(after.getAttribute("d")).toContain("C 75 40 80 40 85 50");
  } finally {
    drawing.discard();
  }
});
