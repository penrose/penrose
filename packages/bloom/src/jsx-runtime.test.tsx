/** @jsxImportSource @penrose/bloom */

import { describe, expect, test } from "vitest";
import { canvas, DiagramBuilder } from "./index.js";
import type { Circle, Group, RawSvgElement, Text } from "./core/types.js";
import { getActiveBuilder, setActiveBuilder } from "./core/builder.js";
import { isRawSvgElement } from "./jsx-runtime.js";

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
    const db = new DiagramBuilder(canvas(400, 400), "test-defs");

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
});
