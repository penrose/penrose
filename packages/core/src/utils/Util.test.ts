import { describe, expect, test } from "vitest";
import { A } from "../types/ast.js";
import {
  EmptyStylePath,
  StylePathToNamespaceScope,
  StylePathToSubstanceScope,
  StylePathToUnindexedObject,
  StylePathToUnnamedScope,
} from "../types/stylePathResolution.js";
import { prettyResolvedStylePath, topsort } from "./Util.js";

describe("topsort", () => {
  test("simple DAG", () => {
    const sorted = topsort(
      (x) => {
        switch (x) {
          case "A":
            return [];
          case "B":
            return ["A"];
          case "C":
            return ["B"];
        }
        throw Error();
      },
      ["C"],
    );
    expect(sorted).toEqual(["A", "B", "C"]);
  });

  test("complex DAG", () => {
    const sorted = topsort(
      (x) => {
        switch (x) {
          case "A":
            return [];
          case "B":
            return ["A"];
          case "C":
            return ["A"];
          case "D":
            return ["E"];
          case "E":
            return ["B", "C"];
        }
        throw Error();
      },
      ["D", "E"],
    );
    expect(sorted).toEqual(["A", "C", "B", "E", "D"]);
  });

  test("disconnected DAG", () => {
    const sorted = topsort(
      (x) => {
        switch (x) {
          case "A":
            return [];
          case "B":
            return ["A"];
          case "C":
            return [];
          case "D":
            return ["C"];
        }
        throw Error();
      },
      ["B", "D"],
    );
    expect(sorted).toEqual(["C", "D", "A", "B"]);
  });
});

describe("pretty printing paths", () => {
  const nodeType: { nodeType: "SyntheticStyle" } = {
    nodeType: "SyntheticStyle",
  };
  test("normal cases", () => {
    const p0: EmptyStylePath<A> = {
      tag: "Empty",
      ...nodeType,
    };
    expect(prettyResolvedStylePath(p0, true)).toEqual("");
    expect(prettyResolvedStylePath(p0, false)).toEqual("");

    const p1: StylePathToSubstanceScope<A> = {
      tag: "Substance",
      ...nodeType,
      styleName: "hello",
      substanceObject: { tag: "SubstanceVar", name: "world" },
    };
    expect(prettyResolvedStylePath(p1, true)).toEqual("hello");
    expect(prettyResolvedStylePath(p1, false)).toEqual("`world`");

    const p2: StylePathToNamespaceScope<A> = {
      tag: "Namespace",
      ...nodeType,
      name: "ns",
    };
    expect(prettyResolvedStylePath(p2, true)).toEqual("ns");
    expect(prettyResolvedStylePath(p2, false)).toEqual("ns");

    const p3: StylePathToUnindexedObject<A> = {
      tag: "Object",
      ...nodeType,
      access: {
        tag: "Member",
        parent: p1,
        name: "xxxx",
      },
    };
    expect(prettyResolvedStylePath(p3, true)).toEqual("hello.xxxx");
    expect(prettyResolvedStylePath(p3, false)).toEqual("`world`.xxxx");

    const p4: StylePathToUnindexedObject<A> = {
      tag: "Object",
      ...nodeType,
      access: {
        tag: "Member",
        parent: p3,
        name: "prop",
      },
    };
    expect(prettyResolvedStylePath(p4, true)).toEqual("hello.xxxx.prop");
    expect(prettyResolvedStylePath(p4, false)).toEqual("`world`.xxxx.prop");

    const p5: StylePathToUnnamedScope<A> = {
      tag: "Unnamed",
      ...nodeType,
      blockId: 123,
      substId: 456,
    };
    expect(prettyResolvedStylePath(p5, true)).toEqual("");
    expect(prettyResolvedStylePath(p5, false)).toEqual("123:456");
  });

  test("edge cases", () => {
    const p1: StylePathToUnindexedObject<A> = {
      tag: "Object",
      ...nodeType,
      access: {
        tag: "Member",
        parent: {
          tag: "Unnamed",
          ...nodeType,
          blockId: 123,
          substId: 456,
        },
        name: "xxxx",
      },
    };
    // notice the lack of dots before `xxxx` when printing user-facing strings
    expect(prettyResolvedStylePath(p1, true)).toEqual("xxxx");
    expect(prettyResolvedStylePath(p1, false)).toEqual("123:456.xxxx");
  });
});
