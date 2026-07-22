import { afterEach, describe, expect, test, vi } from "vitest";
import { DiagramBuilder } from "./builder.js";
import { Diagram } from "./diagram.js";
import { Substance, Type } from "./types.js";
import { canvas } from "./utils.js";

const makeBuilder = () => new DiagramBuilder(canvas(100, 100));

afterEach(() => {
  vi.restoreAllMocks();
});

describe("DiagramBuilder optimization terms", () => {
  test("routes constraints and objectives to separate optimizer inputs", async () => {
    const create = vi.spyOn(Diagram, "create").mockResolvedValue({} as Diagram);
    const builder = makeBuilder();
    const constraint = 1;
    const objective = 2;

    builder.ensure(constraint);
    builder.encourage(objective);
    await builder.build();

    expect(create).toHaveBeenCalledWith(
      expect.objectContaining({
        constraints: [constraint],
        objectives: [objective],
      }),
    );
  });
});

const select = (builder: DiagramBuilder, type: Type): Substance[] => {
  const matches: Substance[] = [];
  builder.forall({ value: type }, ({ value }) => {
    matches.push(value);
  });
  return matches;
};

describe("DiagramBuilder subtyping", () => {
  test("preserves exact matching for flat types", () => {
    const builder = makeBuilder();
    const A = builder.type();
    const B = builder.type();
    const a = A();
    B();

    expect(select(builder, A)).toEqual([a]);
  });

  test("matches direct and transitive subtypes", () => {
    const builder = makeBuilder();
    const A = builder.type();
    const B = builder.type(A);
    const C = builder.type(B);
    const a = A();
    const b = B();
    const c = C();

    expect(select(builder, A)).toEqual([a, b, c]);
    expect(select(builder, B)).toEqual([b, c]);
    expect(select(builder, C)).toEqual([c]);
  });

  test("does not match a supertype or sibling through a subtype selector", () => {
    const builder = makeBuilder();
    const A = builder.type();
    const B = builder.type(A);
    const C = builder.type(A);
    A();
    const b = B();
    C();

    expect(select(builder, B)).toEqual([b]);
  });

  test("matches through multiple supertypes without duplicate callbacks", () => {
    const builder = makeBuilder();
    const A = builder.type();
    const B = builder.type();
    const C = builder.type(A, B);
    const D = builder.type(A, C);
    const c = C();
    const d = D();

    expect(select(builder, A)).toEqual([c, d]);
    expect(select(builder, B)).toEqual([c, d]);
    expect(select(builder, C)).toEqual([c, d]);
  });

  test("applies subtype matching to forallWhere", () => {
    const builder = makeBuilder();
    const A = builder.type();
    const B = builder.type(A);
    const b1 = B();
    const b2 = B();
    const matches: Substance[] = [];

    builder.forallWhere(
      { value: A },
      ({ value }) => value === b2,
      ({ value }) => {
        matches.push(value);
      },
    );

    expect(matches).toEqual([b2]);
    expect(matches).not.toContain(b1);
  });

  test("deduplicates repeated direct supertypes", () => {
    const builder = makeBuilder();
    const A = builder.type();
    const B = builder.type(A, A);
    const b = B();

    expect(select(builder, A)).toEqual([b]);
  });

  test("rejects supertypes declared by another builder", () => {
    const builder = makeBuilder();
    const otherBuilder = makeBuilder();
    const Foreign = otherBuilder.type();

    expect(() => builder.type(Foreign)).toThrowError(
      "Supertypes must be created by the same DiagramBuilder before their subtypes",
    );
  });
});
