/** @jsxImportSource @penrose/bloom */

import { afterEach, describe, expect, test } from "vitest";
import {
  DiagramBuilder,
  getActiveBuilder,
  setActiveBuilder,
  withBuilder,
} from "./core/builder.js";
import type {
  Circle,
  Equation,
  Group,
  Line,
  RawSvgElement,
  Text,
} from "./core/types.js";
import { canvas } from "./core/utils.js";
import { isRawSvgElement, jsx } from "./jsx-runtime.js";

afterEach(() => setActiveBuilder(null));

describe("jsx-runtime", () => {
  test("active builder is set in constructor", () => {
    const db = new DiagramBuilder(canvas(400, 400), "test");
    expect(getActiveBuilder()).toBe(db);
    setActiveBuilder(null); // clean up
  });

  test("circle JSX creates a circle shape with correct r prop", () => {
    const db = new DiagramBuilder(canvas(400, 400), "test");
    const Node = db.type();
    const n = Node();

    db.forall({ n: Node }, ({ n }) => {
      n.icon = <circle r={42} />;
    });

    const shape = n.icon as Circle;
    expect(shape.shapeType).toBe("Circle");
    // r is passed as a literal number, so it should equal 42 directly
    expect(shape.r).toBe(42);
  });

  test("kebab-case props are converted to camelCase", () => {
    const db = new DiagramBuilder(canvas(400, 400), "test");
    const Node = db.type();
    const n = Node();

    db.forall({ n: Node }, ({ n }) => {
      n.icon = <circle r={10} fill-color={[1, 0, 0, 1]} stroke-width={3} />;
    });

    const shape = n.icon as Circle;
    expect(shape.shapeType).toBe("Circle");
    expect(shape.r).toBe(10);
    expect(shape.strokeWidth).toBe(3);
    expect(shape.fillColor).toEqual([1, 0, 0, 1]);
  });

  test("rect maps to rectangle builder method", () => {
    const db = new DiagramBuilder(canvas(400, 400), "test");
    const Node = db.type();
    const n = Node();

    db.forall({ n: Node }, ({ n }) => {
      n.box = <rect width={100} height={50} />;
    });

    expect(n.box.shapeType).toBe("Rectangle");
  });

  test("g maps to group builder method", () => {
    const db = new DiagramBuilder(canvas(400, 400), "test");
    const Node = db.type();
    const n = Node();

    db.forall({ n: Node }, ({ n }) => {
      const c = <circle r={20} />;
      n.grp = <g shapes={[c]} />;
    });

    expect((n.grp as Group).shapeType).toBe("Group");
  });

  test("functional components are called directly", () => {
    const db = new DiagramBuilder(canvas(400, 400), "test");
    const Node = db.type();
    const n = Node();

    const Label = (props: { string?: string }) => (
      <text string={props.string ?? "default"} />
    );

    db.forall({ n: Node }, ({ n }) => {
      n.label = <Label string="hello" />;
    });

    expect((n.label as Text).shapeType).toBe("Text");
    expect((n.label as Text).string).toBe("hello");
  });

  test("throws when used outside builder context", () => {
    setActiveBuilder(null);
    expect(() => {
      return (<circle r={10} />) as unknown;
    }).toThrow("DiagramBuilder context");
  });

  test("unknown SVG elements become RawSvgElements and register as raw defs", () => {
    new DiagramBuilder(canvas(400, 400), "test-defs");

    // Evaluate JSX outside forall — defs are registered as side-effects
    const defsEl = (
      <defs>
        <linearGradient id="grad1" x1="0%" y1="0%" x2="100%" y2="0%">
          <stop offset="0%" stop-color="cornflowerblue" />
          <stop offset="100%" stop-color="tomato" />
        </linearGradient>
      </defs>
    ) as unknown as RawSvgElement;

    expect(isRawSvgElement(defsEl)).toBe(true);
    expect(defsEl.tag).toBe("defs");
    expect(defsEl.children).toHaveLength(1);

    const grad = defsEl.children[0];
    expect(grad.tag).toBe("linearGradient");
    expect(grad.attrs["id"]).toBe("grad1");
    expect(grad.children).toHaveLength(2);
    expect(grad.children[0].attrs["offset"]).toBe("0%");

    setActiveBuilder(null); // clean up
  });

  test("string fill prop on circle becomes rawAttr overriding Penrose fill", () => {
    const db = new DiagramBuilder(canvas(400, 400), "test-fill");
    const Node = db.type();
    const n = Node();

    db.forall({ n: Node }, ({ n }) => {
      n.icon = <circle r={50} fill="url(#grad1)" ensure-on-canvas />;
    });

    const shape = n.icon as Circle;
    // Shape is still a Circle with shapeType
    expect(shape.shapeType).toBe("Circle");
    // rawAttrs carries the raw fill string
    expect(shape.rawAttrs).toBeDefined();
    expect(shape.rawAttrs!["fill"]).toBe("url(#grad1)");
    // ensureOnCanvas is a known field and goes to bloomProps
    expect(shape.ensureOnCanvas).toBe(true);

    setActiveBuilder(null); // clean up
  });

  test("active builder is restored after forall callback", () => {
    const db1 = new DiagramBuilder(canvas(400, 400), "db1");
    const Node = db1.type();
    Node();

    // db1 constructor set active builder to db1; now create db2 to make it db2
    const db2 = new DiagramBuilder(canvas(400, 400), "db2");

    db1.forall({ n: Node }, ({ n }) => {
      // Inside the forall callback, active builder must be db1
      expect(getActiveBuilder()).toBe(db1);
      n.icon = <circle r={5} />;
    });

    // After forall, active builder should be restored (to db2 — what it was before the callback)
    expect(getActiveBuilder()).toBe(db2);
    setActiveBuilder(null); // clean up
  });

  test("withBuilder scopes reusable components and restores context after errors", () => {
    const first = new DiagramBuilder(canvas(100, 100), "first");
    const second = new DiagramBuilder(canvas(100, 100), "second");
    const Dot = ({ r }: { r: number }) => <circle r={r} />;
    const shape = withBuilder(first, () => <Dot r={7} />) as Circle;
    expect(shape.r).toBe(7);
    expect(getActiveBuilder()).toBe(second);
    expect(() =>
      withBuilder(first, () => {
        throw new Error("style failed");
      }),
    ).toThrow("style failed");
    expect(getActiveBuilder()).toBe(second);
  });

  test("withBuilder rejects asynchronous scopes and immediately restores context", () => {
    const first = new DiagramBuilder(canvas(100, 100), "first");
    const second = new DiagramBuilder(canvas(100, 100), "second");
    // JavaScript callers can bypass the compile-time restriction.
    const asynchronous = (() => Promise.resolve()) as unknown as () => void;
    expect(() => withBuilder(first, asynchronous)).toThrow(
      "must be synchronous",
    );
    expect(getActiveBuilder()).toBe(second);
  });

  test("text and equation children flatten nested arrays and conditional content", () => {
    new DiagramBuilder(canvas(100, 100), "labels");
    const text = (
      <text>{["A", [null, 2, false, undefined, "B"]]}</text>
    ) as Text;
    const equation = (
      <equation>
        {"x"}
        {["_", [1]]}
      </equation>
    ) as Equation;
    expect(text.string).toBe("A2B");
    expect(equation.string).toBe("x_1");
    expect(() => jsx("text", { string: "one", children: "two" })).toThrow(
      "either text children",
    );
  });

  test("group children preserve nested shape trees and fragments", async () => {
    const builder = new DiagramBuilder(canvas(100, 100), "groups");
    const Dot = ({ x }: { x: number }) => <circle cx={x} cy={20} r={3} />;
    const group = (
      <g>
        {[
          null,
          false,
          [
            <Dot x={10} />,
            <>
              <Dot x={20} />
              <Dot x={30} />
            </>,
          ],
        ]}
        <g>
          <Dot x={40} />
        </g>
      </g>
    ) as Group;
    expect(group.shapes).toHaveLength(4);
    expect((group.shapes[3] as Group).shapes).toHaveLength(1);
    const diagram = await builder.build();
    try {
      const { svg } = await diagram.render();
      expect(svg.querySelectorAll("circle")).toHaveLength(4);
      expect(svg.querySelectorAll("g")).toHaveLength(2);
    } finally {
      diagram.discard();
    }
  });

  test("native SVG coordinates are optimizer geometry and render at the specified positions", async () => {
    const builder = new DiagramBuilder(canvas(200, 100), "native");
    const x = builder.input({ name: "x", init: 30, optimized: false });
    const circle = (
      <circle cx={x} cy="20px" r="7" fill="#123456" ensure-on-canvas={false} />
    ) as Circle;
    const line = (
      <line
        x1="10"
        y1={15}
        x2={90}
        y2="60px"
        stroke="red"
        stroke-width="2px"
        ensure-on-canvas={false}
      />
    ) as Line;
    <rect
      x={20}
      y={40}
      width="30px"
      height={10}
      fill="blue"
      ensure-on-canvas={false}
    />;
    expect(circle.rawAttrs?.cx).toBeUndefined();
    expect(line.rawAttrs?.x1).toBeUndefined();
    expect(circle.fillColor).toEqual([0x12 / 255, 0x34 / 255, 0x56 / 255, 1]);
    expect(line.strokeColor).toEqual([1, 0, 0, 1]);
    expect(line.strokeWidth).toBe(2);
    const diagram = await builder.build();
    try {
      const { svg } = await diagram.render();
      const renderedCircle = svg.querySelector("circle")!;
      expect(renderedCircle.getAttribute("cx")).toBe("30");
      expect(renderedCircle.getAttribute("cy")).toBe("20");
      const renderedLine = svg.querySelector("line")!;
      expect(renderedLine.getAttribute("x1")).toBe("10");
      expect(renderedLine.getAttribute("y1")).toBe("15");
      expect(renderedLine.getAttribute("x2")).toBe("90");
      expect(renderedLine.getAttribute("y2")).toBe("60");
      const renderedRect = svg.querySelector("rect")!;
      expect(renderedRect.getAttribute("x")).toBe("20");
      expect(renderedRect.getAttribute("y")).toBe("40");
    } finally {
      diagram.discard();
    }
  });

  test("native paint parsing and numeric lengths reject unsupported geometry", () => {
    new DiagramBuilder(canvas(100, 100), "validation");
    const circle = (
      <circle fill="rgba(255, 128, 0, 0.5)" stroke="none" />
    ) as Circle;
    expect(circle.fillColor).toEqual([1, 128 / 255, 0, 0.5]);
    expect(circle.strokeColor).toEqual([0, 0, 0, 0]);
    expect(() => jsx("circle", { cx: "50%", r: 10 })).toThrow(
      "relative CSS units",
    );
    expect(() => jsx("circle", { r: "2em" })).toThrow("relative CSS units");
    expect(() => jsx("circle", { r: Infinity })).toThrow("finite");
    expect(() => jsx("circle", { transform: "translate(1 2)" })).toThrow(
      "unsupported",
    );
    expect(() => jsx("circle", { cx: 10, center: [0, 0] })).toThrow("not both");
    expect(() => jsx("text", { x: 10, string: "A" })).toThrow("unsupported");
    expect(() => jsx("path", { d: "M 0 0 L 1 1" })).toThrow("PathData");
  });

  test("drag and interactive-only props reach the built diagram", async () => {
    const builder = new DiagramBuilder(canvas(100, 100), "interaction");
    const constrain = ([x, y]: [number, number]): [number, number] => [x, y];
    const circle = (
      <circle
        name="draggable"
        r={5}
        drag
        drag-constraint={constrain}
        interactive-only
      />
    ) as Circle;
    expect(circle.drag).toBe(true);
    expect(circle.dragConstraint).toBe(constrain);
    expect(circle.interactiveOnly).toBe(true);
    const diagram = await builder.build();
    try {
      expect(diagram.getDraggingConstraints().get("draggable")).toBe(constrain);
    } finally {
      diagram.discard();
    }
  });

  test("raw definitions flatten arrays and fragments without duplicating descendants", () => {
    new DiagramBuilder(canvas(100, 100), "definitions");
    const defs = (
      <defs>
        <linearGradient id="gradient">
          {[null, [<stop offset="0%" stop-color="red" />]]}
          <>
            <stop offset="100%" stop-color="blue" />
          </>
        </linearGradient>
      </defs>
    ) as RawSvgElement;
    expect(defs.children[0].children).toHaveLength(2);
    expect(() => jsx("svg", { children: <circle r={2} /> })).toThrow(
      "raw SVG elements",
    );
  });

  test("raw paint definitions remain usable with optimized group children", async () => {
    const builder = new DiagramBuilder(canvas(100, 100), "gradient-group");
    <g>
      <defs>
        <linearGradient id="gradient">
          <stop offset="0%" stop-color="red" />
          <stop offset="100%" stop-color="blue" />
        </linearGradient>
      </defs>
      <circle cx={50} cy={50} r={10} fill="url(#gradient)" />
    </g>;
    const diagram = await builder.build();
    try {
      const { svg } = await diagram.render();
      expect(svg.querySelectorAll("defs")).toHaveLength(1);
      expect(svg.querySelectorAll("linearGradient")).toHaveLength(1);
      expect(svg.querySelectorAll("stop")).toHaveLength(2);
      expect(svg.querySelector("g > circle")?.getAttribute("fill")).toBe(
        `url(#${svg.querySelector("linearGradient")!.id})`,
      );
    } finally {
      diagram.discard();
    }
  });

  test("reused definitions are scoped to each rendered diagram", async () => {
    const build = async () => {
      const builder = new DiagramBuilder(canvas(100, 100), "same-source");
      <defs>
        <linearGradient id="shared">
          <stop offset="0%" stop-color="red" />
        </linearGradient>
      </defs>;
      <circle cx={50} cy={50} r={10} fill="url('#shared')" />;
      return builder.build();
    };
    const [first, second] = await Promise.all([build(), build()]);
    try {
      const a = (await first.render()).svg;
      const b = (await second.render()).svg;
      const aId = a.querySelector("linearGradient")!.id;
      const bId = b.querySelector("linearGradient")!.id;
      expect(aId).not.toBe(bId);
      expect(a.querySelector("circle")!.getAttribute("fill")).toBe(
        `url(#${aId})`,
      );
      expect(b.querySelector("circle")!.getAttribute("fill")).toBe(
        `url(#${bId})`,
      );
    } finally {
      first.discard();
      second.discard();
    }
  });
});
