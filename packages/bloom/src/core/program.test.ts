import { afterEach, describe, expect, test, vi } from "vitest";
import { getActiveBuilder, setActiveBuilder } from "./builder.js";
import { Diagram } from "./diagram.js";
import {
  diagram,
  domain,
  proposition,
  type EntityOf,
  type ProgramStyleContext,
} from "./program.js";
import { canvas } from "./utils.js";

const makeDomain = () => {
  const d = domain("sets");
  const Set = d.type("Set");
  const OpenSet = d.type("OpenSet", Set);
  const Point = d.type<"Point", { coordinate: readonly [number, number] }>(
    "Point",
  );
  const Subset = d.predicate("Subset", [Set, Set]);
  const Member = d.predicate("Member", [Point, Set]);
  const Not = d.predicate("Not", [proposition]);
  return d.make({ Set, OpenSet, Point, Subset, Member, Not });
};

afterEach(() => {
  vi.restoreAllMocks();
  setActiveBuilder(null);
});

describe("reusable programs", () => {
  test("typed data composes with inferred subtypes and inherited fields", () => {
    const d = domain("coordinates");
    const Point = d
      .type("Point")
      .withData<{ coordinate: readonly [number, number] }>();
    const MarkedPoint = d
      .type("MarkedPoint", Point)
      .withData<{ note: string }>();
    const SameLocation = d.predicate("SameLocation", [Point, Point]);
    const dom = d.make({ Point, MarkedPoint, SameLocation });
    const s = dom.substance();
    const point = s.Point({ coordinate: [1, 2] });
    const marked = s.MarkedPoint({ coordinate: [1, 2], note: "base point" });
    s.SameLocation(marked, point);
    expect(marked.coordinate).toEqual([1, 2]);
    expect(marked.note).toBe("base point");
    expect(s.make().propositions).toHaveLength(1);
    const checkRequiredFields = () => {
      // @ts-expect-error A subtype inherits its parent's required data.
      s.MarkedPoint({ note: "missing coordinates" });
      // @ts-expect-error New required metadata is enforced too.
      s.MarkedPoint({ coordinate: [1, 2] });
    };
    void checkRequiredFields;
  });

  test("copies and deeply freezes factual data and closes a substance snapshot", () => {
    const dom = makeDomain();
    const s = dom.substance();
    const coordinate: [number, number] = [1, 2];
    const p = s.Point({ label: "p", coordinate });
    const a = s.Set({ label: "A" });
    const member = s.Member(p, a);
    const sub = s.make();
    coordinate[0] = 99;

    expect(p.coordinate).toEqual([1, 2]);
    for (const value of [
      sub,
      sub.entities,
      sub.propositions,
      p,
      p.coordinate,
      member,
      member.args,
    ]) {
      expect(Object.isFrozen(value)).toBe(true);
    }
    expect(() => s.Set()).toThrow("after substance.make()");
    expect(() => s.Member(p, a)).toThrow("after substance.make()");
    expect(() => s.make()).toThrow("after substance.make()");
  });

  test("validates runtime signatures, owners, declarations, and predicate arity", () => {
    const dom = makeDomain();
    const s = dom.substance();
    const other = dom.substance();
    const a = s.Set();
    const foreign = other.Set();
    const point = s.Point({ coordinate: [0, 0] });

    expect(() => s.Subset(a, foreign)).toThrow("this substance");
    // Runtime checks protect JavaScript callers as well as statically typed TS.
    expect(() => Reflect.apply(s.Subset, undefined, [point, a])).toThrow(
      "Expected Set, got Point",
    );
    expect(() => Reflect.apply(s.Subset, undefined, [a])).toThrow(
      "expects 2 arguments",
    );
    const foreignDomain = domain("other");
    expect(() => foreignDomain.type("Other", dom.Set)).toThrow("same domain");
    expect(() => domain("test").type("make")).toThrow("Reserved declaration");
  });

  test("subtype selectors include descendants and ordered selectors retain directed facts", async () => {
    vi.spyOn(Diagram, "create").mockResolvedValue({} as Diagram);
    const dom = makeDomain();
    const s = dom.substance();
    const a = s.Set({ label: "A" });
    const b = s.OpenSet({ label: "B" });
    const c = s.OpenSet({ label: "C" });
    s.Subset(b, a); // Reverse of object insertion order.
    s.Subset(a, a); // Reflexivity is available through direct fact iteration.
    const matches: unknown[] = [];
    const sty = dom.style((ctx) => {
      expect(ctx.entities(dom.Set)).toEqual([a, b, c]);
      expect(ctx.entities(dom.OpenSet)).toEqual([b, c]);
      expect(ctx.facts(dom.Subset)).toEqual([
        [b, a],
        [a, a],
      ]);
      ctx.forallWhere(
        { child: dom.Set, parent: dom.Set },
        ({ child, parent }) => ctx.test(dom.Subset, child, parent),
        ({ child, parent }) => matches.push([child, parent]),
      );
      let orderedCount = 0;
      ctx.forall({ x: dom.Set, y: dom.Set }, () => orderedCount++);
      expect(orderedCount).toBe(6);
    });

    await diagram({ sub: s.make(), sty });
    expect(matches).toEqual([[b, a]]);
  });

  test("nested propositions are expressions, with no unintended positive assertion", async () => {
    vi.spyOn(Diagram, "create").mockResolvedValue({} as Diagram);
    const dom = makeDomain();
    const s = dom.substance();
    const a = s.Set();
    const b = s.Set();
    const subset = s.Subset.expression(a, b);
    const notSubset = s.Not(subset);
    expect(s.Subset.expression(a, b)).toBe(subset);
    expect(s.Subset.test(a, b)).toBe(false);
    expect(s.Not(subset)).toBe(notSubset);
    const foreign = dom.substance();
    expect(() => foreign.Not(subset)).toThrow("this substance");
    const sub = s.make();
    expect(sub.propositions).toEqual([notSubset]);

    await diagram({
      sub,
      sty: dom.style((ctx) => {
        expect(ctx.test(dom.Subset, a, b)).toBe(false);
        expect(ctx.test(dom.Not, subset)).toBe(true);
        expect(ctx.facts(dom.Not)).toEqual([[subset]]);
      }),
    });
  });

  test("parallel assemblies have independent builders and visual views", async () => {
    const create = vi.spyOn(Diagram, "create").mockResolvedValue({} as Diagram);
    const dom = makeDomain();
    const s = dom.substance();
    const a = s.Set({ label: "A" });
    const sub = s.make();
    const contexts: ProgramStyleContext<typeof dom.definitions>[] = [];
    const icons: unknown[] = [];
    const sty = dom.style((ctx) => {
      contexts.push(ctx);
      expect(getActiveBuilder()).toBe(ctx.builder);
      const views = ctx.view(dom.Set, () => ({
        icon: ctx.circle({ r: 10, center: [0, 0] }),
      }));
      icons.push(views.get(a).icon);
      expect(views.has(a)).toBe(true);
    });
    const first = diagram({ sub, sty, variation: "one" });
    const second = diagram({ sub, sty, variation: "two" });
    await Promise.all([second, first]);

    expect(contexts[0].builder).not.toBe(contexts[1].builder);
    expect(icons[0]).not.toBe(icons[1]);
    expect(create).toHaveBeenCalledTimes(2);
    expect(create.mock.calls[0][0].variation).toBe("one");
    expect(create.mock.calls[1][0].variation).toBe("two");
    expect(create.mock.calls[0][0].shapes).toHaveLength(1);
    expect(create.mock.calls[1][0].shapes).toHaveLength(1);
    expect(a).not.toHaveProperty("icon");
    expect(getActiveBuilder()).toBeNull();
  });

  test("composes styles in order and resolves canvas settings explicitly", async () => {
    const create = vi.spyOn(Diagram, "create").mockResolvedValue({} as Diagram);
    const dom = makeDomain();
    const sub = dom.substance().make();
    const order: number[] = [];
    const first = dom.style(
      () => {
        order.push(1);
      },
      { canvas: canvas(100, 100) },
    );
    const second = dom.style(
      () => {
        order.push(2);
      },
      { canvas: canvas(200, 200) },
    );

    await expect(diagram({ sub, sty: [first, second] })).rejects.toThrow(
      "canvases disagree",
    );
    await diagram({ sub, sty: [first, second], canvas: canvas(300, 400) });
    expect(order).toEqual([1, 2]);
    expect(create.mock.calls[0][0].canvas).toMatchObject({
      width: 300,
      height: 400,
    });
  });

  test("rejects foreign styles, forged snapshots, and async style callbacks", async () => {
    vi.spyOn(Diagram, "create").mockResolvedValue({} as Diagram);
    const dom = makeDomain();
    const other = makeDomain();
    const sub = dom.substance().make();
    await expect(diagram({ sub, sty: other.style(() => {}) })).rejects.toThrow(
      "same domain",
    );
    await expect(
      diagram({ sub: { ...sub }, sty: dom.style(() => {}) }),
    ).rejects.toThrow("requires a substance");
    await expect(
      diagram({ sub, sty: dom.style(async () => {}) }),
    ).rejects.toThrow("synchronous");
    expect(getActiveBuilder()).toBeNull();
    await expect(
      diagram({
        sub,
        sty: dom.style((ctx) => {
          ctx.entities(other.Set);
        }),
      }),
    ).rejects.toThrow("substance domain");
  });

  test("rejects asynchronous selectors and view factories before they escape assembly", async () => {
    vi.spyOn(Diagram, "create").mockResolvedValue({} as Diagram);
    const dom = makeDomain();
    const sub = dom.substance();
    sub.Set();
    const facts = sub.make();
    await expect(
      diagram({
        sub: facts,
        sty: dom.style((ctx) => ctx.forall({ set: dom.Set }, async () => {})),
      }),
    ).rejects.toThrow("synchronous");
    await expect(
      diagram({
        sub: facts,
        sty: dom.style((ctx) => {
          ctx.view(dom.Set, async () => ({}));
        }),
      }),
    ).rejects.toThrow("synchronous");
    expect(getActiveBuilder()).toBeNull();
  });
});

test("rejects native asynchronous style callbacks before executing their bodies", async () => {
  vi.spyOn(Diagram, "create").mockResolvedValue({} as Diagram);
  const dom = makeDomain();
  let entered = false;
  const sty = dom.style(async () => {
    entered = true;
  });
  await expect(diagram({ sub: dom.substance().make(), sty })).rejects.toThrow(
    "synchronous",
  );
  expect(entered).toBe(false);
});

/** These compile-time checks must fail for incompatible semantic types. */
const checkProgramTypes = () => {
  const dom = makeDomain();
  const s = dom.substance();
  const a = s.Set();
  const b = s.OpenSet();
  const p = s.Point({ coordinate: [1, 2] });
  s.Subset(b, a); // Subtyping remains assignable statically.
  s.Member(p, a);
  // @ts-expect-error A Point is not a Set.
  s.Subset(p, a);
  // @ts-expect-error Predicate signatures retain tuple arity.
  s.Member(p);
  // @ts-expect-error Factual metadata is required by the constructor.
  s.Point();
  // @ts-expect-error Factual labels are readonly.
  a.label = "changed";
  const foreignDomain = domain("foreign");
  const Foreign = foreignDomain.type("Set");
  const other = foreignDomain.make({ Set: Foreign }).substance();
  // @ts-expect-error Equal type names in another domain have distinct brands.
  s.Subset(other.Set(), a);
  const pType: EntityOf<typeof dom.Point> = p;
  return pType;
};
void checkProgramTypes;
