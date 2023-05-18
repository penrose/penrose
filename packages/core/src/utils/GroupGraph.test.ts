import { describe, expect, test } from "vitest";
import Graph from "./Graph.js";
import { GroupGraph, traverseUp } from "./GroupGraph.js";

describe("Group graph", () => {
  test("traverse up", () => {
    const g: GroupGraph = new Graph<string, number>();
    ["g1", "g2", "g3", "s1", "s2", "s3", "s4", "s5"].map((n) => {
      g.setNode(n, 0);
    });
    g.setEdge({ i: "g1", j: "s1", e: undefined });
    g.setEdge({ i: "g1", j: "g2", e: undefined });
    g.setEdge({ i: "g2", j: "s2", e: undefined });
    g.setEdge({ i: "g2", j: "s3", e: undefined });
    g.setEdge({ i: "g3", j: "s4", e: undefined });
    g.setEdge({ i: "g3", j: "s5", e: undefined });

    const s3Path = traverseUp(g, "s3");
    const s5Path = traverseUp(g, "s5");
    expect(s3Path).toEqual(["s3", "g2", "g1"]);
    expect(s5Path).toEqual(["s5", "g3"]);
  });
});
