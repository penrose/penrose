import Graph from "./Graph";

describe("Graph", () => {
  test("setNode after edge added", () => {
    const g = new Graph<string, string>();
    g.setNode("a", "1");
    g.setNode("b", "2");
    g.setEdge({ i: "a", j: "b", e: undefined });
    expect(g.node("a")).toBe("1");
    g.setNode("a", "3");
    expect(g.node("a")).toBe("3");
    expect(g.outEdges("a").length).toBe(1);
  });

  test("setEdge with missing initial vertex", () => {
    const g = new Graph<string>();
    g.setNode("a", undefined);
    expect(() => g.setEdge({ i: "a", j: "b", e: undefined })).toThrow();
  });

  test("setEdge with missing terminal vertex", () => {
    const g = new Graph<string>();
    g.setNode("b", undefined);
    expect(() => g.setEdge({ i: "a", j: "b", e: undefined })).toThrow();
  });

  test("setEdge with default node label", () => {
    const g = new Graph<string, number>();
    let n = 0;
    g.setEdge({ i: "a", j: "b", e: undefined }, () => ++n);
    expect(g.node("a")).toBe(1);
    expect(g.node("b")).toBe(2);
  });

  const numbersGraph = () => {
    const g = new Graph<number>();
    g.setNode(7, undefined);
    g.setNode(2, undefined);
    g.setNode(9, undefined);
    g.setNode(1, undefined);
    g.setNode(6, undefined);
    g.setNode(5, undefined);
    g.setNode(3, undefined);
    g.setNode(8, undefined);
    g.setNode(4, undefined);
    return g;
  };

  test("topsort with no edges mimics insertion order", () => {
    expect(numbersGraph().topsort()).toEqual([7, 2, 9, 1, 6, 5, 3, 8, 4]);
  });

  const numbersGraphWithEdges = () => {
    const g = numbersGraph();
    g.setEdge({ i: 9, j: 7, e: undefined });
    g.setEdge({ i: 8, j: 9, e: undefined });
    g.setEdge({ i: 1, j: 8, e: undefined });
    g.setEdge({ i: 4, j: 9, e: undefined });
    g.setEdge({ i: 1, j: 7, e: undefined });
    g.setEdge({ i: 9, j: 3, e: undefined });
    g.setEdge({ i: 1, j: 2, e: undefined });
    g.setEdge({ i: 3, j: 7, e: undefined });
    return g;
  };

  test("topsort with edges", () => {
    const g = numbersGraphWithEdges();
    g.setEdge({ i: 5, j: 9, e: undefined });
    expect(g.topsort()).toEqual([1, 8, 4, 5, 9, 3, 7, 2, 6]);
  });

  const numberCycleGraph = () => {
    const g = numbersGraphWithEdges();
    g.setEdge({ i: 9, j: 1, e: undefined });
    return g;
  };

  test("topsort with cycle", () => {
    const g = numberCycleGraph();
    expect(() => g.topsort()).toThrow();
  });

  test("descendants with cycle", () => {
    expect(numberCycleGraph().descendants(8)).toEqual(
      new Set([1, 2, 3, 7, 8, 9])
    );
  });

  test("findCycles with no cycles", () => {
    const g = numbersGraphWithEdges();
    g.setEdge({ i: 5, j: 9, e: undefined });
    expect(g.findCycles()).toEqual([]);
  });

  test("findCycles with one cycle", () => {
    expect(numberCycleGraph().findCycles()).toEqual([[9, 1, 8, 9]]);
  });

  test("removing nodes", () => {
    const g = new Graph<number>();
    g.setNode(1, undefined);
    g.setNode(2, undefined);
    g.setNode(3, undefined);
    g.setEdge({ i: 1, j: 2, e: undefined });

    g.delNode(2);

    expect(g.nodes().sort()).toEqual([1, 3]);
    expect(g.children(1).length).toEqual(0);
  });
});
