import { add, mul } from "@penrose/core";
import { afterEach, describe, expect, test, vi } from "vitest";
import { jsx } from "../jsx-runtime.js";
import { DiagramBuilder, setActiveBuilder, withBuilder } from "./builder.js";
import { Diagram } from "./diagram.js";
import { diagram, domain, type EntityOf } from "./program.js";
import type { Circle } from "./types.js";
import { canvas, namespaceSvgIds } from "./utils.js";

afterEach(() => {
  vi.restoreAllMocks();
  setActiveBuilder(null);
});

describe("program and JSX review regressions", () => {
  test("entity-valued metadata preserves identity and rejects foreign substance references", () => {
    const declarations = domain("maps");
    const Point = declarations.type("Point");
    const Map = declarations.type<"Map", { source: EntityOf<typeof Point> }>(
      "Map",
    );
    const dom = declarations.make({ Point, Map });
    const facts = dom.substance();
    const source = facts.Point({ label: "x" });
    const map = facts.Map({ source });
    expect(map.source).toBe(source);
    const foreign = dom.substance().Point({ label: "other" });
    expect(() => facts.Map({ source: foreign })).toThrow(
      "another substance's objects",
    );
  });

  test("withBuilder rejects native async callbacks before their first statement", () => {
    const builder = new DiagramBuilder(canvas(100, 100), "sync-only");
    let invoked = false;
    expect(() =>
      withBuilder(builder, (async () => {
        invoked = true;
      }) as never),
    ).toThrow("synchronous");
    expect(invoked).toBe(false);
  });

  test("JSX rejects native async components before their first statement", () => {
    let invoked = false;
    expect(() =>
      jsx(
        (async () => {
          invoked = true;
          await Promise.resolve();
          return jsx("circle", { r: 5 });
        }) as never,
        {},
      ),
    ).toThrow("synchronous");
    expect(invoked).toBe(false);
  });

  test("rejecting an async style prevents it from attaching JSX to a previous builder", async () => {
    const create = vi.spyOn(Diagram, "create").mockResolvedValue({} as Diagram);
    const declarations = domain("async-review");
    const dom = declarations.make({});
    const previous = new DiagramBuilder(canvas(100, 100), "previous");
    const style = dom.style(async () => {
      await Promise.resolve();
      jsx("circle", { r: 5 });
    });

    await expect(
      diagram({ sub: dom.substance().make(), sty: style }),
    ).rejects.toThrow("synchronous");
    await previous.build();
    expect(create.mock.calls[0][0].shapes).toHaveLength(0);
  });

  test("async generator styles fail clearly instead of silently omitting their shapes", async () => {
    const declarations = domain("async-generator-review");
    const dom = declarations.make({});
    const style = dom.style(async function* () {
      yield jsx("circle", { r: 5 });
    });
    await expect(
      diagram({ sub: dom.substance().make(), sty: style }),
    ).rejects.toThrow("synchronous");
  });

  test("interactive-only children are omitted by static rendering after grouping", async () => {
    const builder = new DiagramBuilder(canvas(100, 100), "static-group");
    const icon = jsx("circle", { r: 5, "interactive-only": true });
    jsx("g", { children: icon });
    const compiled = await builder.build();
    try {
      const svg = await compiled.renderStatic();
      expect(svg.querySelectorAll("circle")).toHaveLength(0);
    } finally {
      compiled.discard();
    }
  });

  test("drag discovery reaches children of nested reusable components", async () => {
    const builder = new DiagramBuilder(canvas(100, 100), "nested-drag");
    const icon = jsx("circle", { r: 5, drag: true, name: "draggable" });
    const inner = jsx("g", { children: icon });
    jsx("g", { children: inner });
    const compiled = await builder.build();
    try {
      expect(compiled.getDraggingConstraints().has("draggable")).toBe(true);
      const before = await compiled.render();
      const circle = before.svg.querySelector("circle")!;
      const x = Number(circle.getAttribute("cx"));
      const y = Number(circle.getAttribute("cy"));
      compiled.beginDrag("draggable");
      compiled.translate("draggable", 10, 5);
      const after = (await compiled.render()).svg.querySelector("circle")!;
      expect(Number(after.getAttribute("cx"))).toBeCloseTo(x + 10);
      expect(Number(after.getAttribute("cy"))).toBeCloseTo(y - 5);
      compiled.endDrag("draggable");
    } finally {
      compiled.discard();
    }
  });

  test("dragging native SVG input coordinates respects the y-axis conversion", async () => {
    const builder = new DiagramBuilder(canvas(100, 100), "native-move");
    const cx = builder.input({ name: "cx", init: 50 });
    const cy = builder.input({ name: "cy", init: 50 });
    jsx("circle", { name: "native", cx, cy, r: 5, drag: true });
    const compiled = await builder.build();
    try {
      compiled.beginDrag("native");
      compiled.translate("native", 10, 5);
      const circle = (await compiled.render()).svg.querySelector("circle")!;
      expect(Number(circle.getAttribute("cx"))).toBeCloseTo(60);
      expect(Number(circle.getAttribute("cy"))).toBeCloseTo(45);
      expect(compiled.getInput("cx")).toBeCloseTo(60);
      expect(compiled.getInput("cy")).toBeCloseTo(45);
      compiled.endDrag("native");
    } finally {
      compiled.discard();
    }
  });

  test("shared affine endpoint inputs translate once while preserving segment shape", async () => {
    const builder = new DiagramBuilder(canvas(100, 100), "shared-endpoints");
    const x = builder.input({ name: "x", init: 0 });
    const y = builder.input({ name: "y", init: 0 });
    builder.line({
      name: "segment",
      start: [mul(2, x), y],
      end: [add(mul(2, x), 10), add(y, 20)],
      drag: true,
    });
    const compiled = await builder.build();
    try {
      compiled.beginDrag("segment");
      compiled.translate("segment", 10, 5);
      const line = (await compiled.render()).svg.querySelector("line")!;
      expect(Number(line.getAttribute("x1"))).toBeCloseTo(60);
      expect(Number(line.getAttribute("x2"))).toBeCloseTo(70);
      expect(Number(line.getAttribute("y1"))).toBeCloseTo(45);
      expect(Number(line.getAttribute("y2"))).toBeCloseTo(25);
      expect(compiled.getInput("x")).toBeCloseTo(5);
      expect(compiled.getInput("y")).toBeCloseTo(5);
      compiled.endDrag("segment");
    } finally {
      compiled.discard();
    }
  });

  test.each(["fixed", "nonlinear", "cross-axis", "unequal-coefficients"])(
    "unsupported %s draggable coordinates fail clearly",
    async (kind) => {
      const builder = new DiagramBuilder(canvas(100, 100), kind);
      const x = builder.input({ init: 0 });
      const y = builder.input({ init: 0 });
      if (kind === "unequal-coefficients") {
        builder.line({ start: [x, y], end: [mul(2, x), y], drag: true });
      } else {
        builder.circle({
          center:
            kind === "fixed"
              ? [0, 0]
              : kind === "nonlinear"
              ? [mul(x, x), y]
              : [x, x],
          drag: true,
        });
      }
      await expect(builder.build()).rejects.toThrow(/Draggable/);
    },
  );

  test("native SVG coordinates retain drag access to their input variables", async () => {
    const create = vi.spyOn(Diagram, "create").mockResolvedValue({} as Diagram);
    const builder = new DiagramBuilder(canvas(100, 100), "native-drag");
    const cx = builder.input({ name: "cx", init: 50 });
    const cy = builder.input({ name: "cy", init: 50 });
    const icon = jsx("circle", { cx, cy, r: 5, drag: true }) as Circle;
    await builder.build();
    const translated = create.mock.calls[0][0].inputIdxsByPath.get(
      `${icon.name}.center`,
    )!;
    expect(translated).toEqual({
      tag: "Val",
      contents: {
        tag: "VectorV",
        contents: [expect.any(Number), expect.any(Number)],
      },
    });
  });

  test.each(["xlink:href", "aria-controls", "aria-activedescendant"])(
    "SVG id scoping keeps the %s reference connected",
    (attribute) => {
      const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
      const target = document.createElementNS(svg.namespaceURI, "g");
      target.setAttribute("id", "target");
      const reference = document.createElementNS(svg.namespaceURI, "use");
      reference.setAttribute(
        attribute,
        attribute === "xlink:href" ? "#target" : "target",
      );
      svg.append(target, reference);
      namespaceSvgIds(svg, "review", new Set(["target"]));
      expect(reference.getAttribute(attribute)).toBe(
        attribute === "xlink:href" ? "#review--target" : "review--target",
      );
    },
  );
});
